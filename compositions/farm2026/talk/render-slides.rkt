#lang racket

;; Renders the talk deck's score snippets *from the restored PorchFest
;; program* (../../porchfest2026/program.rkt), the same way the paper
;; does — so the slides show the real program, not stand-in art.
;;
;; Each `realize` writes one lilypond PNG into slides-resources/<name>/
;; as a side effect; talk-deck.rkt embeds those via `slide-image`.  Re-run
;;   raco make render-slides.rkt      (from this directory)
;; to regenerate them after editing the program.  Uses look-mom pieces
;; only, so `smallest-violin`'s dancer-writing (dream-sequence) never
;; runs and no repo-root cwd is needed.

(require (except-in tonart direction transpose-octave)
         (except-in "../../scribble/scribble.rkt" insert)
         ;; the canon expander + its staff sign (the paper calls these
         ;; `expand-canon` / `sign`; these are the underlying names)
         (only-in tonart/private/common-practice/cool canon-from-cursor cursor)
         (only-in "../../porchfest2026/program.rkt" program-score)
         (for-syntax racket/base))

;; one single-column, untitled score into slides-resources/<dir>.
;; `#:music-dir` wants a literal, so the path is built at expansion.
(define-syntax (snippet stx)
  (syntax-case stx ()
    [(_ dir body ...)
     (with-syntax ([md (string-append "slides-resources/" (syntax-e #'dir))])
       #'(void
          (realize (scribble-realizer #:emit-title? #f #:numbered? #f
                                      #:section-depth 1 #:columns 1
                                      #:music-dir md)
                   program-score body ...)))]))

;; Row Row Row, zoomed — the condensed canon with its segno signs.
(snippet "rowzoom"
  (@ [(art-section look-mom row-row-row)] (zoom))
  (rewrite-in-music (clef treble)))

;; …expanded into the full three-voice round.
(snippet "rowcanon"
  (@ [(art-section look-mom row-row-row)] (zoom))
  (rewrite-in-music (canon-from-cursor two three)
                    (voice-order [one two three])
                    (clef treble)))

;; Hot Cross Buns in each of the three clefs the players read.
(snippet "hctreble" (@ [(art-section look-mom hot-cross)] (zoom)) (rewrite-in-music (clef treble)))
(snippet "hcalto"   (@ [(art-section look-mom hot-cross)] (zoom)) (rewrite-in-music (clef alto)))
(snippet "hcbass"   (@ [(art-section look-mom hot-cross)] (zoom)) (rewrite-in-music (clef bass)))

;; Twinkle, for the colored-sections slide.
(snippet "twinkle" (@ [(art-section look-mom twinkle)] (zoom)) (rewrite-in-music (clef treble)))

;; Mary Had a Little Lamb — the new section added via append-section.
;; Defined here (as in the paper's demo) since it is not part of the
;; original PorchFest program; rendered on its own.
(define-art mary-had-a-little-lamb
  (key d 0 major)
  (voice@ (one)
    (i@ [0 16]
      (seq (note f 1 4) (note e 0 4) (note d 0 4) (note e 0 4)
           (note f 1 4) (note f 1 4) (note f 1 4)
           (note e 0 4) (note e 0 4) (note e 0 4)
           (note f 1 4) (note a 0 4) (note a 0 4))
      (rhythm 1 1 1 1 1 1 2  1 1 2  1 1 2)
      (apply-rhythm))))

(void
 (realize (scribble-realizer #:emit-title? #f #:numbered? #f
                             #:section-depth 1 #:columns 1
                             #:music-dir "slides-resources/lamb")
          (@ [(art-section lamb)]
             (music mary-had-a-little-lamb (clef treble)))))
