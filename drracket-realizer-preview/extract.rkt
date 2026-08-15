#lang racket/base

;; Subprocess entry point.
;;
;;   racket extract.rkt <mode> <file> [<working-dir>]
;;
;; where <mode> is `scribble` or `strudel`: realize the file's
;; `program` art with that realizer and hand back the result.
;;
;; A `program` is a `define-art` -- a *syntax* binding -- so it cannot
;; be pulled out with `dynamic-require` the way a scribble `doc` can.
;; `realize` is a macro and has to run at compile time in a module that
;; imports the art.  So each mode writes a small driver module next to
;; the source, compiles it, and reads the result back:
;;
;;   #lang scribble/manual
;;   @(require (only-in tonart realize)
;;             (only-in (file "…/scribble.rkt") scribble-realizer)
;;             (only-in (file "…/program.rkt") program))
;;   @(realize (scribble-realizer …) program)
;;
;; Two things about that driver are load-bearing.
;;
;; The `@` prefixes: `#lang scribble/manual` reads its body in text
;; mode, so a bare `(require …)` is literal prose that renders as a
;; paragraph instead of being evaluated.
;;
;; Every import is `only-in`: these modules re-export each other
;; heavily -- scribble.rkt and program-strudel.rkt collide on `sa`,
;; tonart and scribble.rkt collide on `transpose-octave` -- and the
;; hand-written driver scripts in this repo each carry a bespoke
;; `except-in` list to cope.  Naming the one binding we need sidesteps
;; all of it, and hygiene means the realizer's output still resolves
;; against *its* scope, not the driver's.
;;
;; This runs in its own process on purpose.  Expanding one of these
;; programs shells out to lilypond and takes seconds; none of that
;; should be able to wedge or kill the IDE.
;;
;; Everything the program prints while loading -- lilypond's chatter
;; goes to stdout, since scribble.rkt invokes it through `system` -- is
;; captured into a log and kept out of the way, because the result is
;; the only thing allowed on the real stdout.

(require racket/port
         racket/path
         racket/file
         racket/class
         racket/list
         racket/string
         scribble/core
         "scribble-preview.rkt")

(provide extract-result)

;; The art each button realizes.  A per-realizer binding wins over the
;; shared one when the module provides it, so a program that needs
;; something extra in only one of the two outputs can say so:
;;
;;   (define-art program-scribble program (rewrite-in-music (clef treble)))
;;   (define-art program-strudel  program (rewrite-in-music (instrument "supersaw")))
;;
;; The override *replaces* `program` rather than adding to it -- naming
;; `program` first, as above, is how you build on it.  Composing is a
;; token; being unable to replace would not be recoverable.
(define BASE-ART 'program)

(define (override-art mode)
  (case mode
    [(scribble) 'program-scribble]
    [(strudel) 'program-strudel]
    [else #f]))

;; Which binding this run should realize, or #f if the module has
;; neither.
(define (choose-art mode exports)
  (define override (override-art mode))
  (cond
    [(and override (memq override exports)) override]
    [(memq BASE-ART exports) BASE-ART]
    [else #f]))

;; ---------------------------------------------------------------------------
;; locating the shared realizer modules
;; ---------------------------------------------------------------------------

;; Both realizers live under this repo's `compositions/` directory, as
;; `scribble/scribble.rkt` and `strudel/program-strudel.rkt`.  Walk up
;; from the edited file until an ancestor holds both.
(define (find-realizer-root dir)
  (let loop ([d dir])
    (cond
      [(not (path? d)) #f]
      [(and (file-exists? (build-path d "scribble" "scribble.rkt"))
            (file-exists? (build-path d "strudel" "program-strudel.rkt")))
       d]
      [else
       (define-values (parent name dir?) (split-path d))
       (loop (and (path? parent) parent))])))

(define (scribble-module root) (build-path root "scribble" "scribble.rkt"))
(define (strudel-module root) (build-path root "strudel" "program-strudel.rkt"))

;; ---------------------------------------------------------------------------
;; errors
;; ---------------------------------------------------------------------------

(define (format-error e log)
  (define msg
    (if (exn? e)
        (let* ([base (exn-message e)]
               [locs (if (exn:srclocs? e) ((exn:srclocs-accessor e) e) '())]
               [loc (and (pair? locs) (car locs))])
          ;; Syntax errors already carry `file:line:col:` up front; only
          ;; add it for the ones that don't.
          (if (and loc (srcloc-source loc)
                   (not (regexp-match? #px"^[^\\s:]+:[0-9]+:[0-9]+:" base)))
              (format "~a:~a:~a: ~a"
                      (let ([s (srcloc-source loc)])
                        (if (path? s) (path->string (file-name-from-path s)) s))
                      (or (srcloc-line loc) "?")
                      (or (srcloc-column loc) "?")
                      base)
              base))
        (format "~a" e)))
  (list 'err msg log))

;; ---------------------------------------------------------------------------
;; does the module actually export the art we need?
;; ---------------------------------------------------------------------------

;; `define-art` expands to a `define-syntax`, so `program` shows up
;; among the module's phase-0 *syntax* exports.
(define (syntax-exports path)
  ;; `0` makes the module available without visiting or instantiating
  ;; it -- enough to ask what it exports, and cheaper than running the
  ;; body, which the driver will do soon enough anyway.
  (dynamic-require path 0)
  (define-values (vars stxs) (module->exports path))
  (for*/list ([grp (in-list stxs)]
              #:when (eqv? 0 (car grp))
              [e (in-list (cdr grp))])
    (car e)))

;; A miss here is the common case in this repo -- the existing files all
;; call the binding `program-score` -- so say what *is* there rather
;; than letting `only-in` raise a bare "identifier not provided".
(define (missing-art-message path mode names)
  (define override (override-art mode))
  (define candidates
    (sort (map symbol->string
               (filter (lambda (n) (regexp-match? #rx"program|score" (symbol->string n)))
                       names))
          string<?))
  (string-append
   (format "~a defines no `~a`~a.\n\nThe ~a button realizes `~a` if the module provides one, and `~a` otherwise."
           (path->string (file-name-from-path path))
           BASE-ART
           (if override (format " or `~a`" override) "")
           mode override BASE-ART)
   (if (null? candidates)
       ""
       (format "\n\nThis module does provide: ~a\n\nRename one of those to `~a`, or add a `(define-art ~a …)` that composes them."
               (string-join candidates ", ") BASE-ART BASE-ART))))

;; ---------------------------------------------------------------------------
;; driver modules
;; ---------------------------------------------------------------------------

;; `~s` writes each path and the title as a properly escaped Racket
;; literal, so spaces and quotes in either are not a problem.

(define (scribble-driver-text root user-path art title)
  (format (string-append
           "#lang scribble/manual\n"
           "@(require (only-in tonart realize)\n"
           "          (only-in (file ~s) scribble-realizer)\n"
           "          (only-in (file ~s) ~a))\n"
           "@(realize (scribble-realizer #:title ~s #:numbered? #f) ~a)\n")
          (path->string (scribble-module root))
          (path->string user-path)
          art
          title
          art))

(define (strudel-driver-text root user-path art)
  (format (string-append
           "#lang racket\n"
           "(require (only-in tonart realize)\n"
           "         (only-in (file ~s) program-strudel-realizer)\n"
           "         (only-in (file ~s) ~a))\n"
           "(provide strudel-code)\n"
           "(define strudel-code (realize (program-strudel-realizer) ~a))\n")
          (path->string (strudel-module root))
          (path->string user-path)
          art
          art))

;; The driver has to be a sibling of the source: the realizers resolve
;; `#:music-dir` and image paths relative to the current directory, and
;; the source's own relative `require`s have to keep working.
(define (call-with-driver dir suffix text proc)
  (define drv (make-temporary-file (string-append "realizer-driver~a" suffix) #f dir))
  (dynamic-wind
   (lambda () (call-with-output-file drv #:exists 'truncate (lambda (p) (display text p))))
   (lambda () (proc drv))
   (lambda ()
     (with-handlers ([(lambda (_) #t) void]) (delete-file drv))
     ;; `compiled/` picks up a .zo for the driver on the way through.
     (with-handlers ([(lambda (_) #t) void])
       (define-values (base name dir?) (split-path drv))
       (define zo (build-path dir "compiled" (path-replace-extension name #".zo")))
       (when (file-exists? zo) (delete-file zo))))))

;; ---------------------------------------------------------------------------
;; the two modes
;; ---------------------------------------------------------------------------

;; Returns `(ok ART BLOCK …)`, `(code ART STRING)`, or
;; `(err MESSAGE LOG)`.  ART is the binding that actually got realized,
;; which the pane reports -- with two possible sources per button,
;; silence about which one ran would be its own bug.
(define (extract-result mode file [cwd #f])
  (define path (path->complete-path (if (path? file) file (string->path file))))
  (define dir (path-only path))
  (define wd (if cwd (path->complete-path cwd) dir))
  (define log (open-output-string))
  (define (finish-err msg) (list 'err msg (get-output-string log)))
  (with-handlers ([(lambda (_) #t)
                   (lambda (e) (format-error e (get-output-string log)))])
    (parameterize ([current-output-port log]
                   [current-error-port log]
                   [current-directory wd]
                   [current-load-relative-directory dir])
      (define root (find-realizer-root dir))
      (cond
        [(not root)
         (finish-err
          (format (string-append
                   "Could not find the realizer modules.\n\n"
                   "Looked upward from ~a for a directory holding both\n"
                   "  scribble/scribble.rkt\n"
                   "  strudel/program-strudel.rkt")
                  (path->string dir)))]
        [(not (memq mode '(scribble strudel)))
         (finish-err (format "Unknown mode ~a." mode))]
        [else
         (define names (syntax-exports path))
         (define art (choose-art mode names))
         (cond
           [(not art) (finish-err (missing-art-message path mode names))]
           [(eq? mode 'scribble)
            (call-with-driver
             dir ".scrbl" (scribble-driver-text root path art (title-for path))
             (lambda (drv)
               (define doc (dynamic-require drv 'doc (lambda () #f)))
               (if (part? doc)
                   (list* 'ok art (doc->display-list doc))
                   (finish-err "The scribble realizer produced no document."))))]
           [else
            (call-with-driver
             dir ".rkt" (strudel-driver-text root path art)
             (lambda (drv)
               (define s (dynamic-require drv 'strudel-code (lambda () #f)))
               (if (string? s)
                   (list 'code art s)
                   (finish-err "The strudel realizer produced no output."))))])]))))

;; The realizer wants a `#:title`.  Realizing bare means there is no
;; title in the art to take one from, so use the composition
;; directory's name -- that is the piece's name in this repo's layout,
;; and it beats inventing one or falling back to the realizer's own
;; default of "tonart".
(define (title-for path)
  (define dir (path-only path))
  (define-values (base name dir?) (split-path dir))
  (if (path? name) (path->string name) "Program"))

(module+ main
  (define args (current-command-line-arguments))
  (unless (<= 2 (vector-length args) 3)
    (raise-user-error 'extract "expected <mode> <file> [<working-dir>]"))
  (define real-out (current-output-port))
  (write (extract-result (string->symbol (vector-ref args 0))
                         (vector-ref args 1)
                         (and (= 3 (vector-length args)) (vector-ref args 2)))
         real-out)
  (flush-output real-out))
