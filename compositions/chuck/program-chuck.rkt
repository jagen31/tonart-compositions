#lang racket

;; ----------------------------------------------------------------
;; program-chuck-realizer
;;
;; Renders a programmart program -- the same shape `scribble.rkt`
;; and `program-strudel.rkt` consume: `art-section` name-coords with
;; `(music …)` and `(script …)` embeddings and `art-title`s -- into a
;; flat *timeline* of playable events, in document order:
;;
;;   (list 'mark   "Twinkle Twinkle Little Star")
;;   (list 'music  "<chuck source>" 4.0)
;;   (list 'speak  "MARIA" "I'm tired...")
;;   (list 'direct "Maria goes over to the side of the porch.")
;;
;; The timeline is plain data, so the thing that plays it is ordinary
;; Racket -- see the player in drracket-realizer-preview.  That split
;; is deliberate: everything macro-level is testable by printing a
;; list, without a sound card or a chuck process anywhere near it.
;;
;; Why a separate realizer at all: `music-chuck-realizer` only walks
;; the context it is handed, and does not descend into `(music …)`
;; embeddings.  Handing it a whole program yields a valid chuck
;; document containing zero note-ons -- the same reason
;; `program-strudel-realizer` exists rather than reusing
;; `music-strudel-realizer`.  What this realizer does *not* do is
;; re-implement note emission: each block's inner context is handed
;; to `music-chuck-realizer`, which is where the notes come from.
;;
;;   (realize (program-chuck-realizer #:tempo 120) program)
;; ----------------------------------------------------------------

(require (except-in tonart direction transpose-octave)
         "../script/script.rkt"
         ;; `art-title`, `redact`, and the slide objects are
         ;; scribble.rkt's, not script.rkt's.  The `~literal` patterns
         ;; below match by binding identity, so without these the title
         ;; marks silently never fire, redacted fragments drop out of the
         ;; spoken line, and slides never reach the player.
         (only-in "../scribble/scribble.rkt" art-title redact
                  art-slide art-slideshow)
         (for-syntax syntax/parse racket/list racket/string racket/format
                     (only-in art/private/core get-id-ctxt context-ref)
                     art/coordinate/index))

(provide (all-defined-out) (for-syntax (all-defined-out)))

(begin-for-syntax

  ;; The `voice` coordinate on an expr, or #f.  Notes in a program
  ;; carry one via the surrounding `(voice@ (s) …)`.
  (define (voice-of stx)
    (define v (context-ref (get-id-ctxt stx) #'voice))
    (and v (syntax-parse v
             [({~datum voice} n) (syntax-e #'n)]
             [_ #f])))

  (define (note? e)
    (syntax-parse e [({~literal note} _ _ _) #t] [_ #f]))

  (define (safe-interval-end e)
    (with-handlers ([exn:fail? (λ _ 0)]) (expr-interval-end e)))

  ;; `(divisions N)` pins how many context units make a quarter note.
  ;; musicxml-imported blocks declare one; hand-written blocks do not,
  ;; and for those a unit already *is* a quarter.
  (define (block-divisions inner)
    (define d (context-ref inner #'divisions))
    (or (and d (syntax-parse d [({~literal divisions} n:number) (syntax-e #'n)] [_ #f]))
        1))

  ;; How long a block is in its own context units.
  (define (block-units inner)
    (define notes (filter note? inner))
    (max 1 (for/fold ([m 0]) ([n (in-list notes)]) (max m (safe-interval-end n)))))

  ;; Seconds per context unit.  A unit is 1/divisions of a quarter, and
  ;; a quarter is 60/tempo seconds.  Kept exact so `dilate` gets a
  ;; rational.
  (define (seconds-per-unit inner tempo)
    (/ 60 tempo (block-divisions inner)))

  ;; Channels are yours to assign, in the program itself:
  ;;
  ;;   (voice@ (soprano) (channel 1))
  ;;   (voice@ (bass)    (channel 2))
  ;;
  ;; Nothing is injected here -- which instrument a part goes to is a
  ;; decision about the piece, not one this realizer should be guessing.
  ;;
  ;; Be aware that leaving a channel off fails *silently*, and not
  ;; because of anything above: `midi->full-midi` fills in `#f` when
  ;; nothing declares a channel, and the chuck realizer's pattern binds
  ;; `chan:number`, so a channel-less note does not error -- it simply
  ;; fails to match and is dropped.  The player checks for this after
  ;; the fact and says so rather than playing silence.

  ;; The two output paths `music-chuck-realizer` supports are not
  ;; interchangeable, and the difference is the whole ballgame for
  ;; whether anything is audible:
  ;;
  ;;   'audio  note -> tone -> full-tone, which the realizer wires to
  ;;           `SawOsc => dac` -- straight out of chuck's own audio
  ;;           output.  Eight oscillators, so eight-voice polyphony.
  ;;
  ;;   'midi   note -> midi -> full-midi, which the realizer emits as
  ;;           `mout.send(...)` on a MidiOut opened against "IAC Driver
  ;;           Bus 1".  That is *silent* on its own: it needs the IAC
  ;;           port enabled in Audio MIDI Setup and something listening
  ;;           on it.  Right choice if you want to drive a DAW; wrong
  ;;           choice if you just pressed play.
  ;; `spu` leads the chain as a `dilate`, and that is not cosmetic.
  ;; The chuck realizer reads post-`d/dt` instants as *seconds* -- it
  ;; emits `N::ms => now;` straight from them -- so unscaled art units
  ;; play one-second-per-unit no matter what tempo anyone had in mind.
  ;; Scaling the art by the same factor used to compute the block's
  ;; duration is what keeps chuck's clock and the player's wall clock
  ;; from disagreeing; without it blocks overlap by exactly the factor
  ;; between them (2x at divisions=1, 4x at divisions=2).
  ;; `(volume n)` ends up as `n/10 => _oscK.gain` on each of up to
  ;; eight oscillators summing into `dac`, so it is a per-voice gain,
  ;; not a master.  n=5 on an eight-voice block asks for 4.0 and clips
  ;; hard; low single digits is the usable range.  A block that
  ;; declares its own volume keeps it -- this only supplies a default.
  (define (volume-default inner vol)
    (if (context-ref inner #'volume) '() (list #`(volume #,vol))))

  (define (conversion-chain out inner spu vol)
    (cons
     #`(dilate #,spu)
     (case out
       [(midi) (list #'(note->midi) #'(midi->full-midi))]
      ;; `note->tone` needs a `tuning` in context -- tones are
      ;; frequencies, so something has to say how notes map to Hz --
      ;; and `full-tone` carries a volume.  Programs declare neither,
      ;; being notation rather than performance, so supply both.
       [else (append (list #'(tuning 12tet))
                     (volume-default inner vol)
                     (list #'(note->tone) #'(tone->full-tone)))])))

  ;; A script fragment as speakable text.  `redact` keeps its content
  ;; here: the bar is a *visual* device, and silence would be a
  ;; stranger reading of it than just saying the words.
  (define (frag->text f)
    (syntax-parse f
      [s:str (syntax-e #'s)]
      [({~literal redact} t:str) (syntax-e #'t)]
      [_ ""]))

  (define (frags->text fs)
    (string-trim (apply string-append (map frag->text fs))))

  (define (title-of stx)
    (syntax-parse stx [({~literal art-title} t:str) (syntax-e #'t)] [_ #f]))

  ;; The realizer only ever hands a music block's *inner* context to
  ;; `music-chuck-realizer`, so a declaration written at the section or
  ;; program level -- the natural place to put `(channel 1)` once for
  ;; the whole piece -- never reaches the notes, and on the midi path
  ;; that silently drops every note (see `channel-assignments`).  These
  ;; are the declarations that change chuck output and that one would
  ;; reasonably set once outside the music: hoist them into each block.
  (define (ambient-decl? e)
    (syntax-parse e
      [({~literal channel} _) #t]
      [({~literal instrument} _ ...) #t]
      [({~literal tuning} _) #t]
      [({~literal volume} _) #t]
      [_ #f]))

  ;; Prepended, so a block that declares its own wins by being nearer.
  (define (ambient-decls)
    (filter ambient-decl? (current-ctxt)))

  ;; Normalize an `art-slideshow` body into a `#lang slideshow` module
  ;; body -- the same rule the scribble realizer uses so a deck plays
  ;; and prints identically: nested `art-slide`s (optionally under
  ;; `ix--`) become `(slide …)`, raw slideshow forms pass through, and
  ;; `(slide-image "rel")` resolves to an absolute `(bitmap …)` against
  ;; the program module's directory so the player finds it.
  (define (flatten-slide-forms forms)
    (append-map
     (lambda (f)
       (cond
         [(and (pair? f) (eq? (car f) 'ix--)) (flatten-slide-forms (cdr f))]
         [(and (pair? f) (eq? (car f) 'art-slide)) (list (cons 'slide (cdr f)))]
         [else (list f)]))
     forms))
  (define (resolve-slide-images datum src-dir)
    (cond
      [(and (pair? datum) (eq? (car datum) 'slide-image)
            (pair? (cdr datum)) (string? (cadr datum)))
       (list 'bitmap (path->string (path->complete-path (cadr datum) src-dir)))]
      [(pair? datum)
       (cons (resolve-slide-images (car datum) src-dir)
             (resolve-slide-images (cdr datum) src-dir))]
      [else datum]))
  (define (slideshow-module-body forms)
    (define src-dir (or (current-load-relative-directory) (current-directory)))
    (map (lambda (f) (resolve-slide-images f src-dir))
         (flatten-slide-forms forms)))

  ;; Document order comes from the `index` coordinate (set up by the
  ;; `(ix-- …)` around a section body), NOT from raw `(current-ctxt)`
  ;; order: a mapping rewriter like `(hymn-number->score)` runs last and
  ;; appends its music to the tail of the context, so an unsorted walk
  ;; plays every rewritten block after everything authored before it --
  ;; e.g. all the hymn music after all the speech.  `scribble.rkt` sorts
  ;; for exactly this reason; the timeline has to as well.  Exprs with
  ;; no index sort to the end via the `+inf.0` sentinel.
  (define (index<? a b)
    (cond
      [(and (null? a) (null? b)) #f]
      [(null? a) #t]
      [(null? b) #f]
      [(< (car a) (car b)) #t]
      [(> (car a) (car b)) #f]
      [else (index<? (cdr a) (cdr b))]))

  (define (index-key e)
    (define ix (expr-index e))
    (if (null? ix) (list +inf.0) ix)))

(define-art-realizer program-chuck-realizer
  (λ (stx)
    (syntax-parse stx
      [(_ (~alt (~optional (~seq #:tempo tempo:number) #:defaults ([tempo #'120]))
                (~optional (~seq #:out out:id) #:defaults ([out #'audio]))
                (~optional (~seq #:volume vol:number) #:defaults ([vol #'2])))
          ...)
       (define tempo-val (syntax-e #'tempo))
       (define out-val (syntax-e #'out))
       (define vol-val (syntax-e #'vol))
       (define ambient (ambient-decls))
       (define events
         (for/fold ([acc '()] #:result (reverse acc))
                   ([e (in-list (sort (current-ctxt) index<? #:key index-key))])
           (syntax-parse e
             [({~literal music} inner ...)
              (define inner* (syntax->list #'(inner ...)))
              (cond
                [(null? (filter note? inner*)) acc]
                [else
                 (define spu (seconds-per-unit inner* tempo-val))
                 (define secs (exact->inexact (* (block-units inner*) spu)))
                 (cons
                  #`(list 'music
                          (realize (music-chuck-realizer)
                                   #,@ambient
                                   inner ...
                                   #,@(conversion-chain out-val inner* spu vol-val)
                                   (d/dt))
                          #,secs)
                  acc)])]

             [({~literal script} inner ...)
              (for/fold ([acc acc])
                        ([item (in-list (syntax->list #'(inner ...)))])
                (syntax-parse item
                  [({~literal line} ch:id frag ...)
                   (define t (frags->text (syntax->list #'(frag ...))))
                   (if (string=? t "")
                       acc
                       (cons #`(list 'speak
                                     #,(string-upcase (symbol->string (syntax-e #'ch)))
                                     #,t)
                             acc))]
                  [({~literal stage-direction} frag ...)
                   (define t (frags->text (syntax->list #'(frag ...))))
                   (if (string=? t "") acc (cons #`(list 'direct #,t) acc))]
                  [_ acc]))]

             ;; A slide reaches the player as its `#lang slideshow`
             ;; module body -- a list of datums the player writes to a
             ;; file and opens with the slideshow app.  `art-slide` is
             ;; content, so it's wrapped in one `(slide …)`;
             ;; `art-slideshow` is already a module body.
             [({~literal art-slide} inner ...)
              (define content (map syntax->datum (syntax->list #'(inner ...))))
              (cons #`(list 'slide (quote #,(list (cons 'slide content)))) acc)]
             [({~literal art-slideshow} inner ...)
              (define body (slideshow-module-body
                            (map syntax->datum (syntax->list #'(inner ...)))))
              (cons #`(list 'slide (quote #,body)) acc)]

             [_
              (define t (title-of e))
              (if t (cons #`(list 'mark #,t) acc) acc)])))
       #`(list #,@events)])))
