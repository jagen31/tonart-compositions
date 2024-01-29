#lang racket

(require art)


;; outline
(define-syntax section@ (make-rename-transformer #'name@))

;; script
(define-art-object (say []))



(define-art talk-script
  (section@ introduction
    (-- [2 (ix@ 0 (say "this is my presentation"))]
        [2 (ix@ 1 (say "we are going to be sketching with syntax"))])
    (section@ programming-with-data
      (-- 4 [2 (ix@ 2 (say "We will first talk about DATA TYPES which are not syntax"))]
            [2 (ix@ 3 (say "We'll represent a few animals and the sounds they make."))])
      (section@ adt-with-function
        (-- 8
          [2 (ix@ 4 (say "Here is one way to represent it!"))]
          [2 (ix@ 5 (say "Functions written over this type can't be extended to work on other types"))]
          [2 (ix@ 6 (say "Also, knowledge about how to compute the sound is left up to the function"))]))
      (section@ adt-with-function-and-data
        (-- 14
          [2 (ix@ 7 (say "Here is another way."))]
          [2 (ix@ 8 (say "at"))])))))