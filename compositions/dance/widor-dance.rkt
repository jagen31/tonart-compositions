#lang racket

(require tonart "dance-art.rkt")

(define-art phrase-1-dance
  (seq (facing towards) (arm-diagrams [6 6] [3 9] [3 3] [12 12] [3 9] [9 9] [3 3] [12 12] [6 6] [3 3] [9 9] [8 4] [8 8] [4 4])))

(define-art phrase-1-notes
  (seq (notes [f 3] [e 3] [d 3] [e 3] [f 3] [e 3] [d 3] [e 3] [c 3] [c 3] [b 2] [a 2] [b 2] [g 2])))

(define-art phrase-1-rhythm
  (rhythm 2 2 2 2 2 2 2 2 8 2 2 2 2 8))

(define-art phrase-2-dance
  (seq
   (facings t a t t t t t t)
   ;; TODO jagen31 specify what arm is on top in cross
   (arm-diagrams [8 8] [9 9] [3 9] [5.5 3.5] [2 10] [8 4] [4 8] [8 4])))

(define-art phrase-2-notes
  (seq (notes [g 3] [f 3] [e 3] [d 3] [a 3] [g 3] [f 1 3] [e 3])))
(define-art phrase-2-rhythm (rhythm 4 2 2 8 4 2 2 8))


(define-art phrase-3-dance  
  (seq
   (facings a t l r r a l t l r r t t t)
   (arm-diagrams [8 8] [9 9] [5.5 3.5] [10 9] [6 3] [8 4] [9 6] [9 3] [9 9] [3 3] [8 8] [6 6] [2 10] [4 8])))

(define-art phrase-3-notes
  (seq (notes [b -1 3] [a 3] [g 3] [f 3] [g 3] [f 3] [e 3] [d 3] [e 3] [c 1 3] [d 3] [b -1 2] [c 3] [f 2])))
(define-art phrase-3-rhythm (rhythm 4 2 2 2 2 2 2 2 2 2 2 4 4 8))

;; (dr [1500 200] (seq phrase-1) (seq phrase-2) (seq phrase-3))
