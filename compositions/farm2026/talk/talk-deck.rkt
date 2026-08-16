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
   ;; a score sitting on its section's background tint — the same colors
   ;; program-colors declares (mistyrose look-mom, then per piece).
   (define (tinted label p color)
     (define body (vc-append 4 (scale (bt label) 0.6)
                                (scale-to-fit p 700 72)))
     (cc-superimpose
      (colorize (filled-rectangle 760 (+ (pict-height body) 16)) color)
      body))
   ;; a titled tinted section with its score and, appended, a graphic
   (define (section+graphic label score graphic color)
     (define body (vc-append 4 (scale (bt label) 0.6)
                                (scale-to-fit score 620 52)
                                (scale-to-fit graphic 280 42)))
     (cc-superimpose
      (colorize (filled-rectangle 720 (+ (pict-height body) 14)) color)
      body))
   ;; one titled score thumbnail in a running order; `color` #f = plain,
   ;; a color = highlighted (used to mark the newly inserted section).
   (define (piece-row label score color)
     (define body (vc-append 2 (scale (bt label) 0.6)
                                (scale-to-fit score 560 38)))
     (if color
         (cc-superimpose (colorize (filled-rectangle 660 (+ (pict-height body) 12)) color) body)
         body))

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

   (slide #:title "Anatomy of a section"
     (vc-append 12
                (bt "Twinkle Twinkle Little Star")
                (it "Maria gives an introduction.")
                (scale-to-fit (slide-image "slides-resources/twinkle/music-0.cropped.png")
                              720 110))
     (para "art-title names it · script gives the intro · music carries the score"))

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
     (fit (slide-image "slides-resources/rowzoom/music-0.cropped.png"))
     (para "Each sign (segno) marks a new voice entering — a canon"))

   (slide #:title "Expand the canon"
     (para "A Tonart rewriter expands the condensed score into three voices:")
     (code "(rewrite-in-music (expand-canon two three))"))

   (slide #:title "…the full round"
     (fit (slide-image "slides-resources/rowcanon/music-0.cropped.png"))
     (para "Now a stock realizer can play it back as audio"))

   (slide #:title "Task 2: One source, many versions"
     (para "Violin reads treble, viola alto, cello bass:")
     (code "(define-art viola porchfest-program (clef alto))"
           "(define-art cello porchfest-program (clef bass))")
     (para "Edit porchfest-program once; every version follows"))

   (slide #:title "Hot Cross Buns, three clefs"
     (fit (vc-append 16
                     (slide-image "slides-resources/hctreble/music-0.cropped.png")
                     (slide-image "slides-resources/hcalto/music-0.cropped.png")
                     (slide-image "slides-resources/hcbass/music-0.cropped.png"))))

   (slide #:title "Task 3: Compose sub-programs"
     (para "Overlay a color program — no edit to the original:")
     (code "(realize (scribble-realizer)"
           "  porchfest-program color-program)"))

   (slide #:title "…sections line up by coordinate"
     (vc-append 14
                (tinted "Twinkle Twinkle Little Star"
                        (slide-image "slides-resources/twinkle/music-0.cropped.png") (list 255 236 115))
                (tinted "Hot Cross Buns"
                        (slide-image "slides-resources/hctreble/music-0.cropped.png") "tan")
                (tinted "Row Row Row Your Boat"
                        (slide-image "slides-resources/rowzoom/music-0.cropped.png") "lightblue"))
     (para "Each color lands on its section because bg shares its coordinate"))

   (slide #:title "Add graphics at the end"
     (para "A second sub-program of just images, one per section:")
     (code "(define-art graphics-program"
           "  (@ [(section look-mom twinkle)]"
           "     (image \"stars-row.png\"))"
           "  (@ [(section look-mom hot-cross)]"
           "     (image \"buns-grid.png\"))"
           "  (@ [(section look-mom row-row-row)]"
           "     (image \"boat.png\")))"))

   (slide #:title "…one at the end of each section"
     (vc-append 12
                (section+graphic "Twinkle Twinkle Little Star"
                                 (slide-image "slides-resources/twinkle/music-0.cropped.png")
                                 (slide-image "slides-resources/stars.png") (list 255 236 115))
                (section+graphic "Hot Cross Buns"
                                 (slide-image "slides-resources/hctreble/music-0.cropped.png")
                                 (slide-image "slides-resources/buns.png") "tan")
                (section+graphic "Row Row Row Your Boat"
                                 (slide-image "slides-resources/rowzoom/music-0.cropped.png")
                                 (slide-image "slides-resources/boat.png") "lightblue")))

   (slide #:title "Combine with multi-dimensional indices"
     (para "Compose without touching either program — give each an index dimension:")
     (code "(realize (scribble-realizer)"
           "  (ix@ 0 porchfest-program)"
           "  (ix@ 1 graphics-program))")
     (para "index merges by appending a dimension, so every graphic (1.x)")
     (para "sorts strictly after that section's music (0.x) — at the very end"))

   (slide #:title "Add a whole new section"
     (para "Write the new piece as its own little program — title, intro, art, music:")
     (code "(define-art little-lamb-section"
           "  (@ [(section little-lamb)]"
           "    (ix-- (art-title \"Mary Had a Little Lamb\")"
           "          (bg \"lavender\")"
           "          (script (stage-direction \"Mariette …\"))"
           "          (image \"lamb.png\")"
           "          (music mary-had-a-little-lamb …))))"))

   (slide #:title "append-section slots it into the layout"
     (para "A section only shows if the layout lists it — append-section adds it:")
     (code "(@ [(section look-mom)]"
           "  little-lamb-section"
           "  (append-section little-lamb #:after hot-cross))")
     (para "look-mom's layout becomes:")
     (code "(layout twinkle hot-cross little-lamb row-row-row)"))

   (slide #:title "…snug between hot-cross and row"
     (vc-append 8
                (piece-row "Twinkle Twinkle Little Star"
                           (slide-image "slides-resources/twinkle/music-0.cropped.png") (list 255 236 115))
                (piece-row "Hot Cross Buns"
                           (slide-image "slides-resources/hctreble/music-0.cropped.png") "tan")
                (piece-row "Mary Had a Little Lamb"
                           (slide-image "slides-resources/lamb/music-0.cropped.png") "lavender")
                (piece-row "Row Row Row Your Boat"
                           (slide-image "slides-resources/rowzoom/music-0.cropped.png") "lightblue"))
     (para "the new section (lavender) lands exactly where append-section put it"))

   (slide
    (vc-append 24
               (titlet "Modular, extensible, live")
               (t "the art of concert programming")))))
