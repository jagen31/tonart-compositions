#lang racket

(require (except-in tonart direction transpose-octave)
         (prefix-in twink: "twinkle-twinkle.rkt")
         (prefix-in hot: "hot-cross.rkt")
         (prefix-in row: "row-row-row.rkt")
         (prefix-in tail: "swallowtail.rkt")
         tonart/private/common-practice/cool
         (except-in "../scribble/scribble.rkt" insert)
         "../lilypond/arm-diagram.rkt")

(provide (all-defined-out))

(define-art-object (piece []))

(define-art program
  (ix--
    (piece twinkle-twinkle)
    (piece hot-cross-buns)
    (piece row-row-row)
    (piece dream-sequence)
    (piece swallowtail-jig)))

(define-art full-row row:song
  (key d 0 major)
  (time-sig 3 4)
  (i@ [12 12] (cursor))
  (i@ [24 24] (cursor))
  (canon-from-cursor two three)
  (voice-order [one two three]))

(define-art smallest-violin
  (voice@ (violin)
    (i@ [0 16]
      (seq (note b 0 4) (note e 0 4)
           (note c 0 5) (note b 0 4) (note a 0 4) (note g 0 4)
           (note f 1 4) (note d 0 4) (note d 0 5) (note c 0 5)
           (note b 0 4))
      (rhythm 2 2   3/2 1/2 3/2 1/2   1 1 3/2 1/2   4)
      (apply-rhythm))

    ;; Choreography: one quarter-turn per bar.
    (i@ [0 4]   (facing towards))
    (i@ [4 8]   (facing right))
    (i@ [8 12]  (facing away))
    (i@ [12 16] (facing left))

    ;; Arm-diagrams at every note's start time.
    ;; Note starts: 0, 2, 4, 5.5, 6, 7.5, 8, 9, 10, 11.5, 12
    (i@ 0    (arm-diagram 6 6))      ; arms at rest
    (i@ 2    (arm-diagram 9 3))      ; T-pose
    (i@ 4    (arm-diagram 10 4))     ; mime violin under chin
    (i@ 11/2 (arm-diagram 11 3))     ; bowing (beat 5.5)
    (i@ 6    (arm-diagram 10 5))     ; bow back
    (i@ 15/2 (arm-diagram 11 4))     ; bow forward (beat 7.5)
    (i@ 8    (arm-diagram 12 12))    ; arms straight up
    (i@ 9    (arm-diagram 8 4))      ; gesture out right
    (i@ 10   (arm-diagram 4 8))      ; gesture out left
    (i@ 23/2 (arm-diagram 7 5))      ; arms drift down (beat 11.5)
    (i@ 12   (arm-diagram 6 6)))     ; final rest pose

  (arm-diagrams->images "compositions/lilypond/resources"))

;; Section ordering is driven entirely by `(layout …)` art objects
;; — one per parent section path — rather than by `ix--` indices.
;; The realizer's section-grouping pass (cf. scribble.rkt's
;; `group-at`) consults `section->layout` keyed by the parent's
;; full path; declared children render in layout order and the
;; rest fall back to first-occurrence.  `ix--` survives only on
;; sibling *content* (e.g. art-title + script + music inside a
;; leaf section) where index-based ordering is the natural fit.
(define-art program-score
  ;; top-level layout: orders the three direct children of the
  ;; programme itself (their parent path is `()`).
  (layout look-mom dream-sequence celebration)

  (@ [(art-section look-mom)]
    ;; orders look-mom's three sub-pieces.
    (layout twinkle hot-cross row-row-row)
    (art-title "Look mom, I can play a string instrument")

    (@ [(art-section twinkle)]
      (ix--
        (art-title "Twinkle Twinkle Little Star")
        (script (stage-direction "Maria gives an introduction."))
        (music (key d 0 major) twink:song
               ;; lyrics live inline with the notes (rather than
               ;; in a separate overlay) because `(lyrics …)`
               ;; expands to art objects that need to land in
               ;; this `(music …)` block's inner context — which
               ;; only happens if they're physically inside it.
               ;; `voice@ (one)` tags each syllable with the
               ;; voice the lilypond realizer should attach it
               ;; to.  ix-- (set up by `lyrics`) keeps the
               ;; source order stable for matching to notes.
               (voice@ (one)
                 (lyrics
                   "Twin" "kle" "twin" "kle" "lit" "tle" "star"
                   "How" "I" "won" "der" "what" "you" "are"
                   "Up" "a" "bove" "the" "world" "so" "high"
                   "Like" "a" "dia" "mond" "in" "the" "sky"
                   "Twin" "kle" "twin" "kle" "lit" "tle" "star"
                   "How" "I" "won" "der" "what" "you" "are")))))

    (@ [(art-section hot-cross)]
      (art-title "Hot Cross Buns")
      (ix--
        (script (stage-direction "Jared gives an introduction."))
        (music (key d 0 major) hot:song
               (voice@ (one)
                 (lyrics
                   "Hot" "cross" "buns"
                   "Hot" "cross" "buns"
                   "One" "a" "pen" "ny"
                   "two" "a" "pen" "ny"
                   "Hot" "cross" "buns")))))

    (@ [(art-section row-row-row)]
      (art-title "Row Row Row Your Boat")
      (ix--
        (script (stage-direction "Cat gives an introduction."))
        (music (key d 0 major) row:song
               (i@ [12 12] (cursor))
               (i@ [24 24] (cursor))
               (voice@ (one)
                 (lyrics
                   "Row" "row" "row" "your" "boat"
                   "Gent" "ly" "down" "the" "stream"
                   "Mer" "ri" "ly" "mer" "ri" "ly"
                   "mer" "ri" "ly" "mer" "ri" "ly"
                   "Life" "is" "but" "a" "dream"))))))

  (@ [(art-section dream-sequence)]
    ;; no child sections here, so no layout is needed; ix--
    ;; below just orders the three sibling content items
    ;; (art-title / script / music) within this leaf section.
    (art-title "Dream Sequence")
    (ix--
      (script
        (line Maria "I'm tired...")
        (stage-direction "Maria goes over to the side of the porch and lays down.  A blanket is tossed over her.")
        (stage-direction "Tim, cloaked in the garbs of death, enters from stage rear, with a theremin and a folding chair.")
        (stage-direction "Tim tunes the theremin while Cat and Jared lift Maria up.  Maria is in a daze.")
        (stage-direction "A random audience member is selected onstage from the audience.  That audience member is Mariette.")
        (stage-direction "Mariette, Maria, Jared, and Cat dance around the Theremin as the music gets more and more intense.")
        (stage-direction "Maria falls to the ground.  A pause.")
        (line Mariette "I think she's dead.")
        (line Jared
              "And this is what happens when you "
              (redact "drink in public")
              ".  The good people of "
              (redact "7 Hall Ave")
              " porch would like to remind you: No "
              (redact "public drinking")
              ".")
        (stage-direction
          (redact "Jared drinks to this. He hands the beer to ")
          "Tim"
          (redact " who takes a drink, hands it back")
          ", then exits stage rear.")
        (stage-direction "Prep for swallowtail.  Jared and Mariette do World's Smallest Violin on loop while this happens."))

      (music (key e 0 minor) smallest-violin)))

  (@ [(art-section celebration)]
    ;; only one child section today but the layout makes the
    ;; intended ordering explicit (and gives any future
    ;; append-section a layout to extend instead of inventing
    ;; one from scratch).
    (layout swallowtail)
    (art-title "Celebration")

    (@ [(art-section swallowtail)]
      (art-title "Swallowtail Jig")
      (ix--
        (music (key e 0 minor) (time-sig 6 8) (pickup 3/2) tail:song-a (dilate 1/2))
        (music (key e 0 minor) (time-sig 6 8) (i@ 31/2 (line-break))  (pickup 4/2) tail:song-b (dilate 1/2))))))

;; Pure styling overlay — no notes, no prose, just background-color
;; declarations placed into the same `art-section` paths as the
;; main `program-score`.  Realizing `program-score` + `program-colors`
;; together gives the same programme rendered with colored pieces;
;; realizing just `program-score` leaves it uncolored.
(define-art program-colors
  (@ [(art-section look-mom)]
    (bg "mistyrose")
    ;; CSS `palegoldenrod` (#eee8aa) read as brown / olive; this
    ;; custom hex is a clean pastel yellow with no green or amber
    ;; undertone — closer to a "post-it" yellow.
    (@ [(art-section twinkle)]     (bg "#ffec73"))
    (@ [(art-section hot-cross)]   (bg "tan"))
    (@ [(art-section row-row-row)] (bg "lightblue"))))
