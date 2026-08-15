#lang racket

;; The talk deck: how Programmart works, illustrated with the rendered
;; PorchFest snippets from the FARM paper (../paper), copied into
;; slides-resources/ so this is self-contained.  Kept in its own module
;; -- preview-demo.rkt is about the concert program; this is the slides.
;;
;; The body is raw `#lang slideshow` code (art-slideshow passes it
;; through).  `(slide-image "rel")` is resolved at build time to an
;; absolute image path against this file's directory, so both the
;; Scribble pane and the playback deck find the pictures.

(require (except-in tonart direction transpose-octave)
         (except-in "../../scribble/scribble.rkt" insert))

(provide talk-deck)

(define-art talk-deck
  (art-slideshow
   ;; module-level helpers in the generated slideshow
   (define (code . ls)
     (colorize (apply vl-append 6 (map (lambda (s) (scale (tt s) 0.85)) ls))
               "navy"))
   (define (fit p) (scale-to-fit p 840 430))

   (slide
    (vc-append 30
               (titlet "The Art of Concert Programming")
               (t "How Programmart works")))

   (slide #:title "The problem"
     (item "Concert programs are built by pasting rendered scores into a word processor")
     (item "Every variant means another full, hand-maintained copy")
     (item "The rendered media is dead — you can't play it or re-lay it out"))

   (slide #:title "Programmart"
     (item "A specification language for writing concert programs")
     (item "One of a family of extensible “Art languages”")
     (item "Four kinds of form: coordinates, objects, rewriters, embeddings"))

   (slide #:title "Coordinates & the @-form"
     (item "section — hierarchical sections;  index — order within a section")
     (item "The @-form attaches coordinates to objects and scopes rewriters")
     (item "Nested coordinates merge: sections nest, indices gain a dimension"))

   (slide #:title "Embeddings carry a language"
     (item "An embedding holds a whole Art language, not a rendered picture:")
     (subitem "(music …) — Tonart, for scores")
     (subitem "(script …) — Spielart, for stage directions")
     (item "Rendering compiles the embedded language, so it stays live"))

   (slide #:title "Exemplar: the PorchFest program"
     (item "An opening ceremony for Somerville PorchFest 2026")
     (item "Nursery rhymes, a surreal dream sequence, and an Irish jig")
     (item "Written in Programmart, rendered straight to the program"))

   (slide #:title "Task 1: Zoom in"
     (para "Focus the whole program on one section:")
     (code "(@ [(section look-mom row-row-row)]"
           "   (zoom))")
     (para "zoom drops everything outside its coordinates"))

   (slide #:title "The zoomed snippet"
     (fit (slide-image "slides-resources/row-zoom.png"))
     (para "Each sign (segno) marks a new voice entering — a canon"))

   (slide #:title "Expand the canon"
     (para "A Tonart rewriter expands the condensed score into three voices:")
     (code "(rewrite-in-music (expand-canon two three))"))

   (slide #:title "…the full round"
     (fit (slide-image "slides-resources/row-canon.png"))
     (para "Now a stock realizer can play it back as audio"))

   (slide #:title "Task 2: One source, many versions"
     (para "Violin reads treble, viola alto, cello bass:")
     (code "(define-art viola porchfest-program (clef alto))"
           "(define-art cello porchfest-program (clef bass))")
     (para "Edit porchfest-program once; every version follows"))

   (slide #:title "Hot Cross Buns, three clefs"
     (fit (vc-append 16
                     (slide-image "slides-resources/hotcross-treble.png")
                     (slide-image "slides-resources/hotcross-alto.png")
                     (slide-image "slides-resources/hotcross-bass.png"))))

   (slide #:title "Task 3: Compose sub-programs"
     (para "Overlay a color program — no edit to the original:")
     (code "(realize (scribble-realizer)"
           "  porchfest-program color-program)"))

   (slide #:title "…sections line up by coordinate"
     (fit (slide-image "slides-resources/colored.png")))

   (slide #:title "Graphics compose in too"
     (hc-append 40
                (scale-to-fit (slide-image "slides-resources/boat.png") 300 300)
                (vl-append 14
                           (t "append-section drops a graphic")
                           (t "at the end of a section —")
                           (t "again without touching the source.")))
     (blank)
     (t "Modular, extensible, live: the art of concert programming"))))
