#lang racket

;; Sample snippets showing how to use music-lilypond-realizer.
;;
;; Each sample is a `define-art` followed by a `realize` call.  We write the
;; output to a .ly file in this directory so you can run:
;;
;;   lilypond compositions/lilypond/01-scale.ly
;;
;; to render to PDF/MIDI.

(require tonart "lilypond.rkt" "arm-diagram.rkt")

(define (write-ly! filename contents)
  (define path (build-path (path-only (or (variable-reference->module-source
                                           (#%variable-reference)) "."))
                           filename))
  (call-with-output-file path #:exists 'replace
    (λ (out) (display contents out)))
  (printf ";; wrote ~a\n" filename))


;;;; 1. A C-major scale, eighth notes ----------------------------------------
(define-art scale
  (voice@ (right-hand)
    (i@ [0 4]
      (seq (note c 0 4) (note d 0 4) (note e 0 4) (note f 0 4)
           (note g 0 4) (note a 0 4) (note b 0 4) (note c 0 5))
      (uniform-rhythm 1/2) (apply-rhythm))))

(write-ly! "01-scale.ly"
  (realize (music-lilypond-realizer #:title "C Major Scale") scale))


;;;; 2. Two voices in counterpoint -------------------------------------------
(define-art two-voice
  (voice@ (soprano)
    (i@ [0 4]
      (seq (note c 0 5) (note d 0 5) (note e 0 5) (note c 0 5))
      (uniform-rhythm 1) (apply-rhythm)))
  (voice@ (bass)
    (i@ [0 4]
      (seq (note c 0 3) (note g 0 2) (note c 0 3) (note g 0 2))
      (uniform-rhythm 1) (apply-rhythm))))

(write-ly! "02-two-voice.ly"
  (realize (music-lilypond-realizer #:title "Two-Voice") two-voice))


;;;; 3. Mixed durations, rests, accidentals -----------------------------------
(define-art mixed
  (voice@ (lead)
    (i@ [0 1]   (note c 1 4))   ;; C# quarter
    (i@ [1 5/2] (note d 0 4))   ;; D dotted quarter + eighth (= 1.5 beats)
    ;; gap from 5/2 to 3 inserts a rest
    (i@ [3 4]   (note e -1 4))) ;; Eb quarter
  (voice@ (pad)
    (i@ [0 4] (music-rest))))

(write-ly! "03-mixed.ly"
  (realize (music-lilypond-realizer #:title "Mixed") mixed))


;;;; 4. Using the key + scalar degrees, then converting to notes -------------
;; ^s = scalar sequence (degrees of the current key).  ^->note resolves
;; degrees to concrete pitches using the key + octave context.
(define-art tune
  (voice@ (melody)
    (i@ [0 8]
      (key d 0 minor) (octave 4)
      (seq (^s 1 2 3 4 5 4 3 1))
      (uniform-rhythm 1) (apply-rhythm)
      (^->note))))

(write-ly! "04-tune.ly"
  (realize (music-lilypond-realizer #:title "D-minor Tune") tune))


;;;; 5. A small two-part piece with a custom beat unit ------------------------
;; #:beat says how many tonart-interval units equal one quarter note.
;; Here interval units are sixteenth notes (4 per quarter), so #:beat 4.
(define-art ditty
  (voice@ (rh)
    (i@ [0 16]
      (key g 0 major) (octave 5)
      (seq (^s 1 3 5 3 1 3 5 3))
      (uniform-rhythm 2) (apply-rhythm)
      (^->note)))
  (voice@ (lh)
    (i@ [0 16]
      (key g 0 major) (octave 3)
      (seq (^s 1 5 1 5))
      (uniform-rhythm 4) (apply-rhythm)
      (^->note))))

(write-ly! "05-ditty.ly"
  (realize (music-lilypond-realizer #:title "Ditty" #:beat 4) ditty))


;;;; 6. The Lick (the famous jazz cliche, in D minor) -----------------------
;; D E F G E C D, rhythm: eighth eighth eighth eighth eighth quarter eighth.
;; Using #:beat 2 so 1 unit = 1 eighth note.  The phrase fits in one 4/4 bar.
(define-art the-lick
  (voice@ (lick)
    (i@ [0 8]
      (seq (note d 0 4) (note e 0 4) (note f 0 4) (note g 0 4)
           (note e 0 4) (note c 0 4) (note d 0 4))
      (rhythm 1 1 1 1 1 2 1) (apply-rhythm))))

(write-ly! "06-the-lick.ly"
  (realize (music-lilypond-realizer #:title "The Lick" #:beat 2) the-lick))


;;;; 7. Images above the staff -----------------------------------------------
;; The (image "filename") art object renders above the staff at its time.
;; Place one at t=0 and another at t=2 (the second beat of the bar).
(define-art the-lick-with-dudes
  (voice@ (lick)
    (i@ [0 8]
      (seq (note d 0 4) (note e 0 4) (note f 0 4) (note g 0 4)
           (note e 0 4) (note c 0 4) (note d 0 4))
      (rhythm 1 1 1 1 1 2 1) (apply-rhythm))
    ;; images are positioned by their interval start time
    (i@ 0 (image "../../dude.png"))
    (i@ 4 (image "../../dude.png"))))

(write-ly! "07-the-lick-with-dudes.ly"
  (realize (music-lilypond-realizer #:title "The Lick (feat. dudes)" #:beat 2)
           the-lick-with-dudes))


;;;; 8. Arm-diagrams above the staff -----------------------------------------
;; Drop arm-diagram exprs in a voice (with a facing context), then
;; (arm-diagrams->images …) renders each one to a PNG into
;; compositions/lilypond/resources/ and rewrites them into (image …) refs.
;; The realizer then hangs each PNG above the matching note.
(define-art the-lick-with-dance
  (voice@ (lick)
    (facing towards)
    (i@ [0 8]
      (seq (note d 0 4) (note e 0 4) (note f 0 4) (note g 0 4)
           (note e 0 4) (note c 0 4) (note d 0 4))
      (rhythm 1 1 1 1 1 2 1) (apply-rhythm))
    (i@ 0 (arm-diagram 9 3))    ;; arms out (T-pose)
    (i@ 4 (arm-diagram 12 12))  ;; both arms straight up
    (i@ 7 (arm-diagram 6 6)))   ;; both arms straight down
  (arm-diagrams->images "compositions/lilypond/resources" "resources"))

(write-ly! "08-the-lick-with-dance.ly"
  (realize (music-lilypond-realizer #:title "The Lick (with dance)" #:beat 2)
           the-lick-with-dance))


;;;; 9. World's Smallest Violin (Hearts & Flowers, with choreography) -------
;; The theme transposed to E minor — pitches: B E C B A G F# D D C B.  4/4:
;;   bar 1 :  b 2,  e 2                                 (high half, drop to E)
;;   bar 2 :  c 4., b 8, a 4., g 8        (slurred dotted descent)
;;   bar 3 :  f# 4, d 4, d 4., c 8                      (low pattern)
;;   bar 4 :  b 1                                       (whole note resolution)
;; A different arm-diagram lands on every note start, and the dancer's
;; facing rotates one quarter-turn per bar.
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

  (arm-diagrams->images "compositions/lilypond/resources" "resources"))

(write-ly! "09-smallest-violin.ly"
  (realize (music-lilypond-realizer #:title "World's Smallest Violin")
           smallest-violin))


(printf ";; done\n")
