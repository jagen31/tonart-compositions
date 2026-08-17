#lang racket/base

;; Playback subprocess.
;;
;;   racket player.rkt <file> <working-dir> <tempo> <volume> <out>
;;
;; <out> is `audio` (SawOsc -> dac) or `midi` (MidiOut -> IAC Bus 1).
;;
;; Realizes the file's `program-chuck` (falling back to `program`)
;; into a timeline via `program-chuck-realizer`, then performs it:
;; music blocks go to a chuck shell, dialogue goes to `say`.
;;
;; This is a *separate* entry point from extract.rkt because playback
;; is long-running and stateful.  The realizer buttons run a
;; subprocess that exits as soon as it has written a result, which
;; would take the chuck shell down with it.  This process stays alive
;; for the length of the piece and owns the chuck shell.
;;
;; Stopping: the panel closes our stdin.  A reader thread sees the EOF
;; and shuts chuck down cleanly.  That is the graceful path and the
;; one normally taken; the panel keeps a hard kill in reserve, but a
;; hard kill cannot run cleanup here, which is exactly why stdin is
;; the primary signal rather than a signal.
;;
;; Progress is reported a line at a time on stdout so the panel can
;; show where in the piece we are.

(require racket/port
         racket/path
         racket/file
         racket/system
         racket/match
         racket/string
         racket/async-channel
         setup/collection-search
         (only-in tonart do-send-chuck do-advance-time-chuck))

(define CHUCK "/usr/local/bin/chuck")
(define SAY "/usr/bin/say")

;; The slideshow executable ships next to the running `racket`; fall
;; back to PATH, then to a bare name.  Slides are shown in a titled
;; window (not fullscreen) so playback and DrRacket stay reachable.
(define SLIDESHOW
  (let ([cand (and (path? (find-system-path 'exec-file))
                   (build-path (or (path-only (path->complete-path
                                               (find-system-path 'exec-file)))
                                   (current-directory))
                               "slideshow"))])
    (cond
      [(and cand (file-exists? cand)) (path->string cand)]
      [(find-executable-path "slideshow") => path->string]
      [else "slideshow"])))

;; Voice for stage directions, so they are audibly not dialogue.
(define DIRECTION-VOICE "Daniel")

;; Progress goes to the real stdout.  Realizing prints lilypond
;; chatter and `system*` noise, so `current-output-port` gets pointed
;; at stderr for the duration -- this parameter keeps hold of the
;; channel the panel is actually reading.
(define progress-out (make-parameter (current-output-port)))

(define (report fmt . args)
  (displayln (apply format fmt args) (progress-out))
  (flush-output (progress-out)))

;; ---------------------------------------------------------------------------
;; realize the timeline
;; ---------------------------------------------------------------------------

;; Same driver-module trick as extract.rkt, and for the same reason: a
;; `program` is a syntax binding and `realize` is a macro, so the only
;; way to get at it is to compile a module that imports it.
(define (timeline-for path root tempo vol out)
  (define dir (path-only path))
  (define art
    (let ([names (begin (dynamic-require path 0)
                        (let-values ([(vars stxs) (module->exports path)])
                          (for*/list ([g (in-list stxs)] #:when (eqv? 0 (car g))
                                      [e (in-list (cdr g))])
                            (car e))))])
      (cond
        [(memq 'program-chuck names) 'program-chuck]
        [(memq 'program names) 'program]
        [else (error 'player "~a defines no `program` or `program-chuck`."
                     (path->string (file-name-from-path path)))])))
  (define text
    (format (string-append
             "#lang racket\n"
             "(require (only-in tonart realize)\n"
             "         (only-in (file ~s) program-chuck-realizer)\n"
             "         (only-in (file ~s) ~a))\n"
             "(provide chuck-timeline)\n"
             "(define chuck-timeline"
             " (realize (program-chuck-realizer #:tempo ~a #:volume ~a #:out ~a) ~a))\n")
            (path->string (build-path root "chuck" "program-chuck.rkt"))
            (path->string path)
            art tempo vol out art))
  (define drv (make-temporary-file "realizer-player~a.rkt" #f dir))
  (dynamic-wind
   (lambda () (call-with-output-file drv #:exists 'truncate
                (lambda (p) (display text p))))
   (lambda () (values art (dynamic-require drv 'chuck-timeline)))
   (lambda ()
     (with-handlers ([(lambda (_) #t) void]) (delete-file drv))
     (with-handlers ([(lambda (_) #t) void])
       (define-values (base name dir?) (split-path drv))
       (define zo (build-path dir "compiled" (path-replace-extension name #".zo")))
       (when (file-exists? zo) (delete-file zo))))))

;; ---------------------------------------------------------------------------
;; the chuck shell
;; ---------------------------------------------------------------------------

;; `start-chucker` is a macro that hides its output port hygienically,
;; so its ~10 lines of setup are repeated here instead -- the port has
;; to be reachable to send pre-rendered blocks and to shut the shell
;; down on stop.
(define (start-chuck!)
  (define backend
    (lambda (f) (collection-file-path f "tonart" "private" "electronic" "chuck"
                                      "chuck-backend")))
  (match-define (list ck-in ck-out ck-pid ck-err ctl)
    (process* CHUCK "--shell"))
  ;; Drain both pipes; a full buffer would stall the shell.
  (define (drain p) (thread (lambda () (let loop () (when (read-line p) (loop))))))
  (drain ck-in)
  (drain ck-err)
  (for ([f (in-list '("play_queued.ck" "go.ck" "rec.ck"))])
    (displayln (format "+ ~s" (path->string (backend f))) ck-out)
    (flush-output ck-out))
  (sleep 0.5)   ; let the shreds register before anything is queued
  (values ck-out ctl))

(define (speak! text [voice #f])
  (apply system* SAY (append (if voice (list "-v" voice) '()) (list text))))

;; ---------------------------------------------------------------------------
;; slides
;;
;; A `'slide` event carries a `#lang slideshow` module body.  We write
;; it out and open it with the slideshow app in a window, replacing any
;; deck already up so the screen tracks the timeline.  The process is
;; async -- playback keeps going underneath while the slides are shown.
;; ---------------------------------------------------------------------------

(define current-slideshow (box #f))   ; a process control proc, or #f

(define (kill-slideshow!)
  (define ctl (unbox current-slideshow))
  (when ctl (with-handlers ([(lambda (_) #t) void]) (ctl 'kill)))
  (set-box! current-slideshow #f))

(define (show-slides! body)
  (kill-slideshow!)
  (with-handlers ([(lambda (e) #t)
                   (lambda (e)
                     (report "!! slideshow failed: ~a"
                             (if (exn? e) (exn-message e) e)))])
    (define tmp (make-temporary-file "player-slides~a.rkt"))
    (call-with-output-file tmp #:exists 'truncate
      (lambda (p)
        (displayln "#lang slideshow" p)
        (for ([d (in-list body)]) (writeln d p))))
    (define res (process* SLIDESHOW "--size" "800" "600" "--keep-titlebar"
                          (path->string tmp)))
    (set-box! current-slideshow (list-ref res 4))))

;; Block until the open slideshow closes -- i.e. the presenter clicks to
;; the end and quits it -- so the performance holds during the talk
;; instead of playing on underneath it.  Interruptible: a Stop kills the
;; deck and returns, so the player never wedges waiting on a window the
;; user has already given up on.
(define (wait-slideshow! stop?)
  (define ctl (unbox current-slideshow))
  (when ctl
    (let loop ()
      (cond
        [(stop?) (kill-slideshow!)]
        [(eq? 'running (ctl 'status)) (sleep 0.2) (loop)]
        [else (set-box! current-slideshow #f)]))))

;; ---------------------------------------------------------------------------
;; perform
;; ---------------------------------------------------------------------------

;; chuck sequences its own note events off `Go.advanceTime`, but `say`
;; runs here, so this loop keeps a wall clock: sleep out a music
;; block's duration before moving on, and let `say` block for its own.
;; Without that the whole timeline would be flung at chuck in
;; milliseconds and every line would be spoken over the first bar.
;; A block that realized to a chuck document with no note events in it.
;; On the midi path this is the signature of a missing `(channel N)`:
;; `midi->full-midi` fills in `#f`, the realizer's pattern wants a
;; number, and every note is dropped without complaint.  Silence is a
;; terrible diagnostic, so say it out loud.
(define (silent-block? code)
  (not (regexp-match? #px"mout\\.send|=> _osc[0-9]+\\.freq" code)))

(define warned? (box #f))

(define (warn-silent!)
  (unless (unbox warned?)
    (set-box! warned? #t)
    (report "!! That block produced no note events.")
    (report "!! On the MIDI path this usually means the notes have no channel.")
    (report "!! Assign one in the program, e.g. (voice@ (soprano) (channel 1)).")))

;; A one-line summary of an event, for the paused prompt in manual mode.
;; Script lines carry their text so the presenter can read their cue --
;; whether they say it themselves or let `play` speak it.
(define (event-summary ev)
  (match ev
    [(list 'mark label)      (format "~a" label)]
    [(list 'music _ secs)    (format "music · ~a s" (real->decimal-string secs 1))]
    [(list 'speak who text)  (format "~a: ~a" who text)]
    [(list 'direct text)     (format "(stage direction) ~a" text)]
    [(list 'slide _)         "slides"]
    [_                       "item"]))

;; Perform a single event: send music to chuck and sleep out its length,
;; speak dialogue and stage directions, raise a slide deck and hold.
(define (perform-event! ev i total ck-out stop?)
  (match ev
    [(list 'mark label)
     (report "[~a/~a] ~a" i total label)]
    [(list 'music code secs)
     (report "[~a/~a] music ~a s~a" i total (real->decimal-string secs 1)
             (if (silent-block? code) "  ⚠ no events — see below" ""))
     (when (silent-block? code) (warn-silent!))
     (do-send-chuck ck-out code)
     (do-advance-time-chuck ck-out secs)
     (let wait ([left secs])
       (when (and (> left 0) (not (stop?)))
         (sleep (min 0.2 left))
         (wait (- left 0.2))))]
    [(list 'speak who text)
     (report "[~a/~a] ~a" i total who)
     (speak! text)]
    [(list 'direct text)
     (report "[~a/~a] (direction)" i total)
     (speak! text DIRECTION-VOICE)]
    [(list 'slide body)
     (report "[~a/~a] slides — holding until the deck is closed" i total)
     (show-slides! body)
     (wait-slideshow! stop?)]
    [_ (void)]))

;; Automatic: walk the timeline end to end, letting each event run for
;; its own length.
(define (perform-auto! timeline total ck-out stop?)
  (for ([ev (in-list timeline)] [i (in-naturals 1)])
    (unless (stop?)
      (perform-event! ev i total ck-out stop?))))

;; Manual: pause before every performable item and wait for a command --
;; `play` performs the current item and leaves us on it (so it can be
;; repeated), `next`/`prev` step forward/back, `stop` ends.  Section
;; marks carry no sound, so they are announced and stepped past rather
;; than paused on.  Every other item is the presenter's choice: `play`
;; to let the computer perform it, or step past having done it yourself.
;;
;; Indexed rather than list-walked, because `prev` has to reach items
;; already behind the cursor.
(define (perform-manual! vec total ck-out stop? next-cmd)
  (define (mark-at? j) (and (pair? (vector-ref vec j)) (eq? 'mark (car (vector-ref vec j)))))
  ;; Step one performable item from `from` in direction `dir` (+1 / -1),
  ;; announcing any section marks crossed.  Returns the new index, or #f
  ;; if the walk runs off that end.
  (define (step from dir)
    (let scan ([j (+ from dir)])
      (cond
        [(or (< j 0) (>= j total)) #f]
        [(mark-at? j)
         (perform-event! (vector-ref vec j) (add1 j) total ck-out stop?)
         (scan (+ j dir))]
        [else j])))
  (define first (step -1 +1))
  (when first
    (let loop ([cur first])
      (unless (stop?)
        (define ev (vector-ref vec cur))
        (report "[~a/~a] ⏸ ~a — Play to perform · Prev / Next to move"
                (add1 cur) total (event-summary ev))
        (let wait ()
          (case (next-cmd)
            [(play)
             (perform-event! ev (add1 cur) total ck-out stop?)
             (unless (stop?)
               (report "[~a/~a] ✓ ~a — Prev / Next to move" (add1 cur) total (event-summary ev)))
             (wait)]
            [(next) (define n (step cur +1)) (if n (loop n) (void))]
            [(prev) (define p (step cur -1)) (if p (loop p) (wait))]
            [(stop) (void)]
            [else (wait)]))))))

(define (perform! timeline ck-out stop? manual? next-cmd)
  (define total (length timeline))
  (if manual?
      (perform-manual! (list->vector timeline) total ck-out stop? next-cmd)
      (perform-auto! timeline total ck-out stop?)))

(module+ main
  (define args (current-command-line-arguments))
  (unless (= 6 (vector-length args))
    (raise-user-error 'player "expected <file> <working-dir> <tempo> <volume> <out> <mode>"))
  (define path (path->complete-path (string->path (vector-ref args 0))))
  (define wd (path->complete-path (string->path (vector-ref args 1))))
  (define tempo (string->number (vector-ref args 2)))
  (define vol (string->number (vector-ref args 3)))
  (define out (vector-ref args 4))
  (define manual? (string=? "manual" (vector-ref args 5)))

  (define root
    (let loop ([d (path-only path)])
      (cond
        [(not (path? d))
         (error 'player "could not find compositions/chuck/program-chuck.rkt above ~a"
                (path->string path))]
        [(file-exists? (build-path d "chuck" "program-chuck.rkt")) d]
        [else (define-values (p n dir?) (split-path d)) (loop (and (path? p) p))])))

  (define stopped (box #f))
  (define cmd-ch (make-async-channel))
  ;; The panel talks to us over stdin, one line per command: `play`
  ;; performs the current item, `next`/`prev` move between items, and
  ;; closing the port (EOF) -- or a `stop` line -- ends the piece.  In
  ;; automatic mode the panel only ever closes the port, so this
  ;; collapses to the old stop-on-EOF protocol.
  ;; `void` matters: a module-level expression's value gets printed,
  ;; and a stray #<thread:...> on stdout corrupts the progress stream
  ;; the panel parses.
  (void
   (thread (lambda ()
             (let loop ()
               (define l (read-line))
               (cond
                 [(eof-object? l)
                  (set-box! stopped #t) (async-channel-put cmd-ch 'stop)]
                 [else
                  (case (string-trim l)
                    [("play") (async-channel-put cmd-ch 'play) (loop)]
                    [("next") (async-channel-put cmd-ch 'next) (loop)]
                    [("prev") (async-channel-put cmd-ch 'prev) (loop)]
                    [("stop")
                     (set-box! stopped #t) (async-channel-put cmd-ch 'stop)]
                    [else (loop)])])))))

  (define ck-out #f)
  (define ctl #f)
  (define (shutdown!)
    (kill-slideshow!)
    (when ctl (with-handlers ([(lambda (_) #t) void]) (ctl 'kill)))
    (set! ctl #f))

  (parameterize ([progress-out (current-output-port)]
                 [current-directory wd]
                 [current-output-port (current-error-port)])
    (with-handlers ([(lambda (e) #t)
                     (lambda (e)
                       (shutdown!)
                       (report "!! ~a" (if (exn? e) (exn-message e) e))
                       (exit 1))])
      (define-values (art timeline) (timeline-for path root tempo vol out))
      (report "~a · ~a events · ~a" art (length timeline) out)
      (set!-values (ck-out ctl) (start-chuck!))
      (perform! timeline ck-out (lambda () (unbox stopped))
                manual? (lambda () (async-channel-get cmd-ch)))
      (report (if (unbox stopped) "stopped" "done"))
      (shutdown!)
      (exit 0))))
