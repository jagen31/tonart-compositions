#lang racket/base

;; The right-hand pane: a toolbar plus a read-only `text%` showing the
;; realized program.
;;
;; Two buttons, one per realizer.  `Scribble` renders the concert
;; program as a document into the pane.  `Strudel ▸` realizes the
;; .strudel source and hands it straight to a Strudel REPL on
;; localhost -- the source is never displayed here; the pane is for
;; reading the program, and a wall of generated JavaScript is not
;; that.
;;
;; Realizing happens in a subprocess (see extract.rkt).  The panel keeps
;; the resulting display list around so that resizing and zooming only
;; re-run layout, never the program.

(require racket/class
         racket/gui/base
         framework
         racket/port
         racket/path
         racket/file
         racket/string
         racket/tcp
         racket/runtime-path
         net/base64
         net/uri-codec
         net/sendurl
         setup/dirs
         "layout.rkt")

(provide preview-panel% strudel-port strudel-url)

(define-runtime-path extract-module "extract.rkt")
(define-runtime-path player-module "player.rkt")

;; Playback tempo, in BPM.  A bare `program` carries no tempo, and the
;; timeline needs one to turn note intervals into seconds.
(define play-tempo 120)

;; Oscillator gain, remembered across sessions.  2 rather than 5: the
;; value is per-voice and up to eight voices sum into chuck's dac, so 5
;; asks for 4.0 and clips into the speech.
(preferences:set-default 'realizer-preview:volume 2
                         (lambda (x) (and (exact-integer? x) (<= 1 x 10))))

;; Output path for playback: 'audio (SawOsc -> dac) or 'midi (MidiOut
;; -> "IAC Driver Bus 1").
(preferences:set-default 'realizer-preview:out 'audio
                         (lambda (x) (memq x '(audio midi))))

;; Where a local Strudel REPL is expected.  `npm run dev` in a strudel
;; checkout serves the Astro site here.
(define strudel-port 4321)

;; DrRacket is a GUI app, so `racket` is not on its PATH in any useful
;; sense; go straight to the installation's own binary.
(define racket-exe
  (let* ([bin (find-console-bin-dir)]
         [exe (and bin (build-path bin (if (eq? 'windows (system-type)) "racket.exe" "racket")))])
    (if (and exe (file-exists? exe)) exe (find-executable-path "racket"))))

;; ... and for the same reason lilypond, which the scribble realizer
;; shells out to, is not on it either.  A GUI app launched from Finder
;; inherits a bare PATH that has neither Homebrew nor /usr/local on it.
(define (env-with-tool-paths)
  (define e (environment-variables-copy (current-environment-variables)))
  (unless (eq? 'windows (system-type))
    (define cur (bytes->string/utf-8 (or (environment-variables-ref e #"PATH") #"") #\?))
    (define extra
      (for/list ([d (in-list '("/opt/homebrew/bin" "/usr/local/bin" "/opt/local/bin"))]
                 #:when (and (directory-exists? d)
                             (not (regexp-match? (regexp (regexp-quote d)) cur))))
        d))
    (unless (null? extra)
      (environment-variables-set!
       e #"PATH" (string->bytes/utf-8 (string-join (cons cur extra) ":")))))
  e)

;; ---------------------------------------------------------------------------
;; strudel REPL handoff
;; ---------------------------------------------------------------------------

;; Strudel reads its program out of the URL fragment.  From the
;; strudel source (packages/core/util.mjs):
;;
;;   code2hash(code) = encodeURIComponent(btoa(utf8Bytes(code)))
;;
;; `uri-unreserved-encode` escapes a little more than
;; encodeURIComponent does, but only characters decodeURIComponent
;; reverses anyway, and it covers the `+` `/` `=` that base64 emits and
;; a fragment would otherwise mangle.
(define (strudel-url code)
  (define b64 (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 code) #"")))
  (format "http://localhost:~a/#~a" strudel-port (uri-unreserved-encode b64)))

(define (strudel-server-up?)
  (with-handlers ([(lambda (_) #t) (lambda (_) #f)])
    (define-values (i o) (tcp-connect "localhost" strudel-port))
    (close-input-port i)
    (close-output-port o)
    #t))

;; ---------------------------------------------------------------------------

(define preview-canvas%
  (class editor-canvas%
    (init-field notify-resize)
    (super-new)
    (define/override (on-size w h)
      (super on-size w h)
      (notify-resize))))

(define preview-panel%
  (class vertical-panel%
    ;; `get-source` returns (list path-or-#f text modified?) for the
    ;; frame's current tab, or #f if there is nothing to realize.
    (init-field get-source)
    (super-new [stretchable-width #t])

    (define blocks #f)          ; cached display list
    (define zoom 1.0)
    (define running #f)         ; the live subprocess, if any
    ;; `running` is only set once the subprocess actually exists, which
    ;; leaves a window where an auto tick could start a second run.
    ;; This flag is set synchronously instead.
    (define in-flight? #f)
    (define generation 0)       ; discards results from cancelled runs
    (define last-rendered #f)   ; source text the cache corresponds to
    (define last-seen #f)       ; source text at the previous auto tick
    (define last-change-ms 0)
    (define outcome #f)         ; #f while never run / in flight, else 'ok or 'error
    (define mode 'scribble)     ; realizer the last / next run uses
    (define realized-art #f)    ; which binding the last run actually used
    (define strudel-code #f)    ; last handed-off source; kept for introspection only
    (define player #f)          ; the playback subprocess, if any
    (define player-in #f)       ; its stdin -- closing it is the stop signal
    ;; Which working directory this file's realizers want.  Most
    ;; compositions build from their own directory, but some reach for
    ;; repo-root-relative paths -- widor's `load-musicxml` does -- so a
    ;; failed run is retried from the repo root and the answer is
    ;; remembered per file.
    (define cwd-cache (make-hash))

    ;; ---- toolbar ----
    ;;
    ;; Two rows, not one.  A horizontal-panel's minimum width is the sum
    ;; of its children's, so packing every control into one row set a
    ;; large floor on the pane -- and the pane is half the window's
    ;; split, so the whole window inherited it.  Splitting makes the
    ;; floor the *wider row* instead of the total.
    (define bar (new horizontal-panel%
                     [parent this]
                     [stretchable-height #f] [border 0] [spacing 2]
                     [alignment '(left center)]))
    (define bar2 (new horizontal-panel%
                      [parent this]
                      [stretchable-height #f] [border 0] [spacing 2]
                      [alignment '(left center)]))
    (define bar3 (new horizontal-panel%
                      [parent this]
                      [stretchable-height #f] [border 0] [spacing 2]
                      [alignment '(left center)]))

    (define scribble-button
      (new button% [label "Scribble"] [parent bar]
           [callback (lambda (b e) (render-now 'scribble))]))

    (define strudel-button
      (new button% [label "Strudel ▸"] [parent bar]
           [callback (lambda (b e) (strudel-run))]))

    (define play-button
      (new button% [label "Play ▸"] [parent bar]
           [callback (lambda (b e) (toggle-play))]))

    ;; Which output path the chuck realizer uses.  Not a cosmetic
    ;; choice: Tone is `SawOsc => dac`, audible on its own; MIDI is
    ;; `MidiOut` on "IAC Driver Bus 1", which is silent unless that port
    ;; is enabled in Audio MIDI Setup and something is listening.
    (define out-choice
      (new radio-box% [label #f] [parent bar2]
           [choices '("Tone" "MIDI")]
           [style '(horizontal)]
           [selection (if (eq? 'midi (preferences:get 'realizer-preview:out)) 1 0)]
           [callback (lambda (r e)
                       (preferences:set 'realizer-preview:out (out-mode))
                       (sync-out-controls!))]))

    (define auto-box
      (new check-box% [label "Auto"] [parent bar2]
           [callback (lambda (b e) (set-auto (send b get-value)))]))

    ;; Per-voice oscillator gain, as `(volume n)` -> `n/10 => gain` in
    ;; chuck.  Baked in when the timeline is realized, so it takes
    ;; effect on the next Play, not mid-piece.
    (define volume-slider
      (new slider% [label "Vol"] [parent bar2]
           [min-value 1] [max-value 10]
           [init-value (preferences:get 'realizer-preview:volume)]
           [min-width 70] [stretchable-width #f]
           [callback (lambda (s e)
                       (preferences:set 'realizer-preview:volume (send s get-value)))]))

    ;; A slider, not −/+ buttons: macOS floors every button at ~84px, so
    ;; two cost 168px of pane width.  The range has to stay small,
    ;; though -- a slider's width scales with its number of steps (a
    ;; 50-300 range renders ~190px wide), so this indexes into a levels
    ;; list rather than being a raw percentage.
    (define zoom-levels (vector 0.5 0.6 0.75 0.9 1.0 1.25 1.5 2.0 3.0))
    (define zoom-slider
      (new slider% [label "Zoom"] [parent bar3]
           [min-value 1] [max-value (vector-length zoom-levels)] [init-value 5]  ; 1.0
           [stretchable-width #f]
           [callback (lambda (s e)
                       (set-zoom (vector-ref zoom-levels (sub1 (send s get-value)))))]))

    ;; Stretchable with a tiny minimum so a long status ("program-chuck
    ;; · 27 events · midi") stretches to fill the row but never widens
    ;; the pane -- it clips instead.
    (define status-message
      (new message% [label ""] [parent bar3]
           [stretchable-width #t] [auto-resize #f] [min-width 40]))

    ;; ---- document ----
    (define doc-text (new preview-text%))
    (send doc-text hide-caret #t)
    (send doc-text set-styles-sticky #f)
    (send doc-text auto-wrap #t)
    (send doc-text lock #t)

    (define canvas
      (new preview-canvas%
           [parent this]
           [editor doc-text]
           [style '(auto-vscroll no-hscroll)]
           [notify-resize (lambda () (schedule-relayout))]))

    ;; Resizing fires a burst of on-size calls; only the last matters.
    (define relayout-timer
      (new timer% [notify-callback (lambda () (relayout))] [just-once? #t]))

    (define auto-timer
      (new timer% [notify-callback (lambda () (auto-tick))]))

    (define/private (schedule-relayout)
      (when blocks (send relayout-timer start 120 #t)))

    (define/private (status s) (send status-message set-label s))

    (define/private (usable-width)
      (define-values (w h) (send canvas get-client-size))
      (max 120 w))

    (define/private (relayout)
      (when blocks
        (render-display-list! doc-text blocks (usable-width) zoom)))

    (define/private (set-zoom z)
      (set! zoom (max 0.5 (min 3.0 z)))
      (relayout))

    (define/private (set-auto on?)
      (cond
        [on?
         (set! last-seen #f)
         (send auto-timer start 700)]
        [else (send auto-timer stop)]))

    ;; Re-run once the source has been quiet for a moment, using
    ;; whichever realizer was last chosen.  Polling the buffer rather
    ;; than hooking `after-save-file` means unsaved edits are realized
    ;; too, and it touches nothing internal to DrRacket.
    (define/private (auto-tick)
      (define src (get-source))
      (when src
        (define text (cadr src))
        (cond
          [(equal? text last-seen)
           ;; Scribble only.  The strudel button hands off to the
           ;; browser, and re-firing that on every pause would open a
           ;; tab per keystroke-burst.
           (when (and (eq? mode 'scribble)
                      (not in-flight?)
                      (not (equal? text last-rendered))
                      (>= (- (current-inexact-milliseconds) last-change-ms) 1200))
             (render-now 'scribble))]
          [else
           (set! last-seen text)
           (set! last-change-ms (current-inexact-milliseconds))])))

    (define/private (kill-running)
      (when running
        (with-handlers ([(lambda (_) #t) void]) (subprocess-kill running #t))
        (set! running #f)))

    (define/private (set-buttons-enabled! on?)
      (send scribble-button enable on?)
      (send strudel-button enable on?))

    ;; The strudel source is never shown in the pane -- the button
    ;; realizes and hands the result straight to the REPL.  Check the
    ;; server up front rather than after: realizing takes ~10 s, and
    ;; there is nothing to do with the result if nothing is listening.
    (define/private (strudel-run)
      (cond
        [(strudel-server-up?) (render-now 'strudel)]
        [else
         (message-box
          "Strudel"
          (format (string-append
                   "Nothing is listening on localhost:~a.\n\n"
                   "Start a Strudel REPL there first — in a strudel checkout:\n\n"
                   "    npm run dev\n\n"
                   "Then press Strudel again.")
                  strudel-port)
          #f '(ok caution))]))

    ;; ---- playback ----------------------------------------------------
    ;;
    ;; Unlike the two realizer buttons, this owns a process that outlives
    ;; the call: player.rkt holds a chuck shell open for the length of
    ;; the piece.  Stopping closes its stdin, which is the graceful path
    ;; and the one that lets it take chuck down with it -- a hard kill
    ;; would leave chuck orphaned and still holding the audio device.

    (define/private (out-mode)
      (if (= 1 (send out-choice get-selection)) 'midi 'audio))

    ;; The midi path sends a fixed velocity (0x78), so the gain slider
    ;; does nothing there -- disable it rather than leave a control that
    ;; silently has no effect.
    (define/private (sync-out-controls!)
      (send volume-slider enable (eq? 'audio (out-mode))))

    (define/private (set-play-label!)
      (send play-button set-label (if player "Stop ■" "Play ▸")))

    (define/private (toggle-play)
      (if player (stop-play) (start-play)))

    (define/private (stop-play)
      (when player-in
        (with-handlers ([(lambda (_) #t) void]) (close-output-port player-in))
        (set! player-in #f)
        (status "Stopping…"))
      ;; Backstop only: if it has not gone away on its own, something is
      ;; wedged and an orphaned chuck is the lesser problem.
      (define sp player)
      (when sp
        (thread (lambda ()
                  (unless (sync/timeout 5 sp)
                    (with-handlers ([(lambda (_) #t) void])
                      (subprocess-kill sp #t)))))))

    (define/private (start-play)
      (define-values (src dir cleanup) (materialize))
      (cond
        [(not src) (status "Save this file to a directory first.")]
        [else
         (define cwd (hash-ref cwd-cache (path->string src) dir))
         (define-values (sp out in err)
           (parameterize ([current-directory dir]
                          [current-environment-variables (env-with-tool-paths)])
             (subprocess #f #f #f racket-exe
                         (path->string player-module)
                         (path->string src)
                         (path->string cwd)
                         (number->string play-tempo)
                         (number->string (send volume-slider get-value))
                         (symbol->string (out-mode)))))
         (set! player sp)
         (set! player-in in)
         (set-play-label!)
         (status "Starting playback…")
         ;; Drain stderr; the realize phase is chatty and a full pipe
         ;; would stall the player.
         (void (thread (lambda ()
                         (let loop () (unless (eof-object? (read-line err)) (loop))))))
         (void
          (thread
           (lambda ()
             (let loop ()
               (define l (read-line out))
               (unless (eof-object? l)
                 (queue-callback
                  (lambda () (when (eq? sp player) (status l))) #f)
                 (loop)))
             (subprocess-wait sp)
             (cleanup)
             (queue-callback
              (lambda ()
                (when (eq? sp player)
                  (set! player #f)
                  (set! player-in #f)
                  (set-play-label!)))
              #f))))]))

    ;; Write the buffer somewhere the program's own relative `require`s
    ;; and `define-runtime-path`s still resolve -- which means a sibling
    ;; of the real file, not a temp directory.  A clean buffer skips
    ;; this and uses the real file, so the compiled/ cache applies.
    (define/private (materialize)
      (define src (get-source))
      (cond
        [(not src) (values #f #f void)]
        [else
         ;; DrRacket's `get-filename` is absolute, but the subprocess
         ;; runs with its own current directory, so anything relative
         ;; that slipped through would get resolved twice.
         (define path (let ([p (car src)]) (and p (path->complete-path p))))
         (define text (cadr src))
         (define modified? (caddr src))
         (cond
           [(not path) (values #f #f void)]
           [(not modified?) (values path (path-only path) void)]
           [else
            (define dir (path-only path))
            (define ext (or (path-get-extension path) #".rkt"))
            (define tmp (make-temporary-file
                         (string-append "realizer-preview~a" (bytes->string/utf-8 ext #\?))
                         #f dir))
            (call-with-output-file tmp #:exists 'truncate/replace
              (lambda (o) (display text o)))
            (values tmp dir (lambda () (with-handlers ([(lambda (_) #t) void])
                                         (delete-file tmp))))])]))

    ;; Introspection, used by the frame and by tests.
    (define/public (render-outcome) outcome)
    (define/public (block-count) (if blocks (length blocks) 0))
    (define/public (status-text) (send status-message get-label))
    (define/public (doc-editor) doc-text)
    (define/public (strudel-text) strudel-code)
    (define/public (current-mode) mode)
    (define/public (current-art) realized-art)

    (define/public (render-now [m mode])
      (kill-running)
      (set! mode m)
      (set! outcome #f)
      (set! generation (add1 generation))
      (define seq generation)
      (define-values (src dir cleanup) (materialize))
      (cond
        [(not src)
         (set! blocks #f)
         (render-message! doc-text
                          "Save this file to a directory, then pick a realizer.\n\nEach button realizes `program-scribble` / `program-strudel` if the module provides one, and `program` otherwise."
                          zoom)
         (status "")]
        [else
         (define text (cadr (get-source)))
         (define key (path->string src))
         (set! in-flight? #t)
         (status (format "Realizing (~a)…" m))
         (set-buttons-enabled! #f)
         (define started (current-inexact-milliseconds))
         (void
          (thread
           (lambda ()
             (define result (run-with-cwd-fallback m src dir key))
             (cleanup)
             (define elapsed (/ (- (current-inexact-milliseconds) started) 1000.0))
             (queue-callback
              (lambda ()
                (when (= seq generation)
                  (set! running #f)
                  (set! in-flight? #f)
                  (set-buttons-enabled! #t)
                  (set! last-rendered text)
                  (case (car result)
                    [(ok)
                     (set! strudel-code #f)
                     (set! realized-art (cadr result))
                     (set! blocks (cddr result))
                     (set! outcome 'ok)
                     (relayout)
                     (status (format "~a · ~a blocks · ~a s"
                                     realized-art (length blocks)
                                     (real->decimal-string elapsed 1)))]
                    [(code)
                     ;; Straight to the browser.  `blocks` is deliberately
                     ;; left as-is, so a document already in the pane
                     ;; survives firing the strudel button.
                     (define s (caddr result))
                     (set! realized-art (cadr result))
                     (set! strudel-code s)
                     (set! outcome 'ok)
                     (send-url (strudel-url s))
                     (status (format "~a · ~a chars · opened in Strudel · ~a s"
                                     realized-art (string-length s)
                                     (real->decimal-string elapsed 1)))]
                    [else
                     (set! strudel-code #f)
                     (set! realized-art #f)
                     (set! blocks #f)
                     (set! outcome 'error)
                     (render-error! doc-text (cadr result) (caddr result) zoom)
                     (status "Error")])))
              #f))))]))

    ;; Try the file's own directory first, then the repo root.  Report
    ;; the *first* failure if both fail: the file's directory is the
    ;; usual convention, so its error is the more relevant one.
    (define/private (run-with-cwd-fallback m src dir key)
      (define (attempt cwd)
        (run-extract m src dir cwd (lambda (sp) (set! running sp))))
      (define cached (hash-ref cwd-cache key #f))
      (cond
        [cached (attempt cached)]
        [else
         (define first-result (attempt dir))
         (cond
           [(not (eq? 'err (car first-result)))
            (hash-set! cwd-cache key dir)
            first-result]
           [else
            (define root (find-repo-root dir))
            (cond
              [(or (not root) (equal? root dir)) first-result]
              [else
               (define second-result (attempt root))
               (cond
                 [(eq? 'err (car second-result)) first-result]
                 [else (hash-set! cwd-cache key root) second-result])])])]))

    (define/public (ensure-rendered)
      (unless (or blocks in-flight?)
        (render-message! doc-text
                         "Scribble renders the program as a document here.\n\nStrudel \u25b8 realizes the .strudel source and opens it in a REPL on localhost:4321.\n\nEach realizes `program-scribble` / `program-strudel` if the module provides one, and `program` otherwise."
                         zoom)))

    (define/public (shutdown)
      (send auto-timer stop)
      (set! generation (add1 generation))   ; discard anything still in flight
      (set! in-flight? #f)
      (kill-running)
      ;; Closing the frame has to stop playback, and has to give the
      ;; player long enough to shut chuck down -- otherwise chuck
      ;; outlives DrRacket still holding the audio device.
      (define sp player)
      (stop-play)
      (when sp (sync/timeout 5 sp)))

    (sync-out-controls!)

    (render-message! doc-text
                     "Scribble renders the program as a document here.\n\nStrudel \u25b8 realizes the .strudel source and opens it in a REPL on localhost:4321.\n\nEach realizes `program-scribble` / `program-strudel` if the module provides one, and `program` otherwise."
                     zoom)))

;; The shapes extract.rkt promises:
;;   (ok ART BLOCK ...)   (code ART STRING)   (err MESSAGE LOG)
;; Checked rather than assumed: everything downstream indexes into this
;; positionally, so a shape it does not expect becomes a crash inside
;; layout instead of a message in the pane.
(define (well-formed-result? v)
  (and (list? v)
       (pair? v)
       (case (car v)
         [(ok) (and (>= (length v) 2)
                    (symbol? (cadr v))
                    (andmap pair? (cddr v)))]
         [(code) (and (= 3 (length v)) (symbol? (cadr v)) (string? (caddr v)))]
         [(err) (and (= 3 (length v)) (string? (cadr v)))]
         [else #f])))

;; Nearest ancestor holding a .git -- the working directory the
;; root-relative compositions expect.
(define (find-repo-root dir)
  (let loop ([d dir])
    (cond
      [(not (path? d)) #f]
      [(directory-exists? (build-path d ".git")) d]
      [else
       (define-values (parent name dir?) (split-path d))
       (loop (and (path? parent) parent))])))

;; Run extract.rkt and return its RESULT.
(define (run-extract mode src dir cwd note-subprocess)
  (with-handlers ([(lambda (_) #t)
                   (lambda (e)
                     (list 'err
                           (if (exn? e) (exn-message e) (format "~a" e))
                           ""))])
    (unless racket-exe
      (error 'preview "cannot find the `racket` executable"))
    (parameterize ([current-directory dir]
                   [current-environment-variables (env-with-tool-paths)]
                   [current-subprocess-custodian-mode 'kill])
      (define-values (sp out in err)
        (subprocess #f #f #f racket-exe
                    (path->string extract-module)
                    (symbol->string mode)
                    (path->string src)
                    (path->string cwd)))
      (note-subprocess sp)
      (close-output-port in)
      ;; Drain both pipes concurrently; a full stderr buffer would
      ;; otherwise deadlock a program that logs a lot.
      (define ob (box ""))
      (define eb (box ""))
      (define t1 (thread (lambda () (set-box! ob (port->string out)))))
      (define t2 (thread (lambda () (set-box! eb (port->string err)))))
      (subprocess-wait sp)
      (thread-wait t1)
      (thread-wait t2)
      (close-input-port out)
      (close-input-port err)
      (define o (unbox ob))
      (define code (subprocess-status sp))
      (cond
        [(and (eqv? 0 code) (not (string=? (string-trim o) "")))
         (with-handlers ([(lambda (_) #t)
                          (lambda (_)
                            (list 'err "Could not read the realizer output."
                                  (string-append o "\n" (unbox eb))))])
           (define v (read (open-input-string o)))
           (if (well-formed-result? v)
               v
               ;; Most likely cause: this panel and extract.rkt are from
               ;; different builds, because DrRacket was left running
               ;; across a `raco make`.  Say so rather than letting the
               ;; mismatch surface as a contract violation from deep in
               ;; the layout code.
               (list 'err
                     (string-append
                      "Unexpected realizer output.\n\n"
                      "If the plugin was rebuilt while DrRacket was running, "
                      "restart DrRacket -- the pane and the realizer subprocess "
                      "are out of sync.")
                     o)))]
        [(eqv? 0 code)
         (list 'err "The realizer process produced no output." (unbox eb))]
        [else
         (list 'err (format "The realizer process exited with status ~a." code)
               (string-append o "\n" (unbox eb)))]))))
