#lang racket

(require "lib.rkt" "chuck.rkt" (only-in pict bitmap scale-to-fit) (except-in tonart staff-realizer bitmap))


(define-art lecture-scenes
  ;; lecture content
  (name@ tonart
         (seq
          (ix--
           (name@ folksies
                  (ix--
                   (file "folksy.mscz")
                   (file "folksy.rkt")))
           (name@ intros
                  (ix--
                   (file "intro-example.rkt")
                   (file "intro-details.rkt"))))))
  (name@ flashback
         (seq (ix--
               (name@ bsl
                      (ix--
                       (file "form-bsl.rkt")
                       (file "score-bsl.rkt")))
               (name@ isl
                      (ix--
                       (file "form-isl.rkt")
                       (file "score-isl.rkt")))
               (name@ asl
                      (ix--
                       (file "form-asl.rkt")
                       (file "score-asl.rkt")))
               (name@ aux
                      (ix-- (file "submod-example.rkt")
                            (file "pre-modules.png"))))))
  (name@ reveal
         (seq (ix--
               (file "lecture2.rkt")
               (file "forms-n-scores.rkt"))))
  (name@ death-by-ppt
         (seq (ix--
               (name@ old-score
                      (ix--
                       (file "form-example.rkt")
                       (file "score-example.rkt")))
               (name@ new-score
                      (ix--
                       (file "make-piece.rkt")
                       (file "piece.rkt")))
               (name@ ppt
                      (ix--
                       (file "art_of_syntax.mscz")
                       (file "the-end.rkt")))))))

(define-art timings
  (name@ tonart (number 25))
  (name@ flashback (number 15))
  (name@ reveal (number 10))
  (name@ death-by-ppt (number 15)))

;;;;;;;;;;;;;;;;;;;;;
;; THE FOUR ARTIFACTS

(define folksy-tonart (scale-to-fit (bitmap "folksy.png") 800 500))

(define modules-art (scale-to-fit (bitmap "modules.png") 800 500))

(define file-layout-art (scale-to-fit (bitmap "files.png") 800 500))

(define ppt-score (scale-to-fit (bitmap "ppt.png") 800 500))

(provide (all-defined-out))

#|
(!rkt lecture-scenes (name@ <name> (zoom)) (inline-seq) (file->open-code))


(show [1200 300] (namespace lecture-scenes))
|#

(define lambda-image (bitmap "lambda.png"))