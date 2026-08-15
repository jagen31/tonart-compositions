#lang racket
(require tonart "lilypond.rkt")

(define-art tune
  (voice@ (v)
    (i@ [0 16]
      (key c 0 major) (octave 4)
      (seq (^s 1 3 5 8) (^s 9 8 5 3)
           (^s 1 3 5 8) (^s 7 5 3 1))
      (uniform-rhythm 1) (apply-rhythm)
      (^->note))))

(displayln (realize (music-lilypond-realizer #:title "tune") tune))
