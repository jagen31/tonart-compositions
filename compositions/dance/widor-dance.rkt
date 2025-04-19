#lang racket

(require tonart "dance-art.rkt")

(define-art phrase-1
  (facing towards)
  (arm-diagrams [6 6] [3 9] [3 3] [12 12] [3 9] [9 9] [3 3] [12 12] [6 6] [3 3] [9 9] [8 4] [8 8] [4 4]))

(define-art phrase-2
  (facings t a t t t t t t)
  ;; TODO jagen31 specify what arm is on top in cross
  (arm-diagrams [8 8] [9 9] [3 9] [5.5 3.5] [2 10] [8 4] [4 8] [8 4]))

(define-art phrase-3
  (facings a t l r r a l t l r r t t t)
  (arm-diagrams [8 8] [9 9] [5.5 3.5] [10 9] [6 3] [8 4] [6 6] [9 3] [9 9] [3 3] [8 8] [6 6] [2 10] [4 8]))

;; (dr (seq phrase-1) (seq phrase-2) (seq phrase-3))
