#lang racket

;; Scratch file for trying the Realizer Preview pane -- delete whenever.
;;
;; Each button realizes a per-realizer binding if the module provides
;; one, and `program` otherwise:
;;
;;   Scribble -> program-scribble, else program
;;   Strudel  -> program-strudel,  else program
;;
;; The status line says which one it used.
;;
;; Try it:
;;   View -> Show Realizer Preview   (or shift-cmd-R)
;;   Scribble  -> the concert program: scores, colored sections, script
;;   Strudel   -> the .strudel source, then Copy or Open
;;
;; Both take about 10 s, because lilypond runs.

(require (except-in tonart direction transpose-octave)
         datenart
         "hymnal.rkt"
         "resources.rkt"
         (except-in "../../scribble/scribble.rkt" insert)
         ;; dance vocabulary (facing, arm-diagram, arm-diagrams->images);
         ;; re-exports ../dance/dance-art.rkt.
         "../../lilypond/arm-diagram.rkt")

(provide program program-scribble program-strudel program-chuck
         program-title)

;; Overrides the directory-name default the preview uses for the
;; document title.
(define program-title "The Art of Concert Programming")

;; The shared base -- what either button falls back to.
(define-art farm-bass
  (voice@ [bass]
    (seq (ix--
     (music (i@ [0 6] (music-rest)))
     (music
      (seq (^s 5 -2 1 1 5 -2 0 0 3 1 4 2 5 -2 1))
      nettleton-a-rhythm (apply-rhythm))
     (music
      (seq (^s 5 -2 1 1 5 -2 0 0 3 1 4 2 5 -2 1))
      nettleton-a-rhythm (apply-rhythm))
     (music
      (seq (^s 3 2 1 1 3 4 1 1 1 1 3 2 1 1 3 4 1 1 1))
      nettleton-b-rhythm (apply-rhythm))
     (music
      (seq (^s 5 -2 1 1 5 -2 0 0 3 1 4 2 5 -2 1))
      nettleton-a-rhythm (apply-rhythm))))
    (inline-music-seq)
    (key d 0 major)
    (octave 4)
    (^->note)
    (dilate 1/2)))

(define-art program-prelude
  (@ [(art-section prelude)]
     (ix--
      (art-title "Prelude")
      (script
       (line Presenter "Welcome to my Art demo!"))
      (resource prelude))))

(define-art program-opening-hymn
  (@ [(art-section opening-hymn)]
     (ix--
      (art-title "Opening Hymn")
      (bg "goldenrod")
      (script
       (line Presenter "Hello everyone.")
       (line Audience "Hello <Presenter>.")
       (line Presenter
             "Please rise and join us in singing, in full voice, song number 3, `Come to FARM and hear our music'.")
       (stage-direction "the audience rises"))
      (voice@ [main] (hymn-number 3)))))

;; The closing number: a short, cheerful D-major hook and a dead-easy
;; dance -- always face the audience, three poses (rest / arms out /
;; arms up), one per beat, peaking with arms up on the high A.  Built
;; like `smallest-violin` in program.rkt: melody in `main`, a `facing`
;; over the whole span, and an `arm-diagram` at each note's start.  The
;; arm-diagram -> image conversion is left to `program-scribble`, so a
;; chuck or strudel run doesn't write EPS dancers it has no use for.
(define-art program-finale
  (@ [(art-section finale)]
     (ix--
      (art-title "Finale — Everybody Dance!")
      (bg "goldenrod")
      (script
       (line Presenter "And now, the grand finale! On your feet — follow the dancers: rest, arms out, arms UP!")
       (stage-direction "the troupe leads the whole porch in the dance"))
      (music
       (key d 0 major)
       (voice@ (main)
         (i@ [0 9]
           (seq (note a 0 4) (note d 0 5) (note e 0 5) (note f 1 5)
                (note a 0 5) (note f 1 5) (note e 0 5) (note d 0 5))
           (rhythm 1 1 1 1  1 1 1 2)
           (apply-rhythm))
         ;; easy dance: never turn, just three arm shapes
         (i@ [0 9] (facing towards))
         (i@ 0 (arm-diagram 6 6))     ; rest
         (i@ 1 (arm-diagram 9 3))     ; arms out
         (i@ 2 (arm-diagram 12 12))   ; arms up
         (i@ 3 (arm-diagram 9 3))     ; arms out
         (i@ 4 (arm-diagram 12 12))   ; arms UP on the high A
         (i@ 5 (arm-diagram 9 3))     ; arms out
         (i@ 6 (arm-diagram 9 3))     ; arms out
         (i@ 7 (arm-diagram 6 6))))))) ; rest

;; The talk itself -- the slide deck lives in talk-deck.rkt.
(define-art program-talk
  (@ [(art-section slides)]
     (ix--
      (art-title "Talk")
      (resource slides))))

(define-art program
  hymnal
  resources
  (ix--
   program-prelude
   program-opening-hymn
   program-talk
   program-finale)
)
  

;; Scribble-only: the section background colors.  Comment this out and
;; press Scribble again; the bands disappear, because the button falls
;; back to `program`, and the status line says `program` instead of
;; `program-scribble`.
(define-art program-scribble
  program
  (hymn-number->title)
  (resource->title)
  ;; Turn the finale's arm-diagrams into EPS dancer figures the
  ;; lilypond realizer can hang above the staff.  One-arg form writes
  ;; to ./resources (cwd is this file's dir, same as pldi's musicxml)
  ;; with absolute refs, so the .ly still finds them after scribble
  ;; cd's into scribble-resources to compile.
  (rewrite-in-music
   (arm-diagrams->images "resources"))
)

;; Strudel-only: tag every note with a synth voice.  The colors would
;; do nothing here, and the instrument would do nothing in the PDF --
;; which is the point of splitting them.
(define-art program-strudel
  program
  (rewrite-in-music
   (instrument "square")))

;; ChucK-only.  Play walks the program in order: each section's music
;; goes to a chuck shell as SawOsc tones, and every `line` /
;; `stage-direction` in a `script` is spoken by `say`.
;;
;; Tempo is the knob that matters -- it decides how many seconds a
;; context unit is worth, and the player sleeps for exactly as long as
;; chuck is playing, so the two stay in step.  The panel passes 120 BPM
;; for now.
(define-art program-chuck
  program
  (hymn-number->score)
  (resource->contents)
  (rewrite-in-music
   (voice@ [bass] (clef bass)))
  (@ [(art-section opening-hymn)]
     (rewrite-in-music
      farm-bass
      (dilate 3/2)))
  (channel 1)
  (voice@ [main] (channel 1))
  (voice@ [bass] (channel 9)))

;; Naming `program` first is how you build on it.  An override replaces
;; the base rather than adding to it, so a `program-scribble` that did
;; not mention `program` would realize only the colors.
