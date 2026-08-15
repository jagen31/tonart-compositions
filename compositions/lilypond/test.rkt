#lang racket

(require tonart "lilypond.rkt")

(define-art demo
  (voice@ (soprano)
    (i@ [0 4]
      (seq (note c 0 4) (note d 0 4) (note e 0 4) (note g 0 4))
      (uniform-rhythm 1) (apply-rhythm)))
  (voice@ (bass)
    (i@ [0 4]
      (seq (note c 0 3) (note c 0 3) (note g 0 2) (note c 0 3))
      (uniform-rhythm 1) (apply-rhythm))))

(define-art demo2
  (voice@ (sop)
    (i@ [0 1]   (note c 1 4))
    (i@ [1 5/2] (note d 0 4))
    (i@ [3 4]   (note e 0 4)))
  (voice@ (alt)
    (i@ [0 4] (music-rest))))

(displayln (realize (music-lilypond-realizer #:title "Demo") demo))
(displayln (realize (music-lilypond-realizer #:title "Demo2") demo2))
