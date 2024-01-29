#lang racket

(require tonart/base tonart/private/common-practice/lib art/sequence/ravel)

(define-art my-bass (music (seq (^s 3 5 1 5 1)) (rhythm 2 1 2 1 3)))
(define-art my-melody (music (seq (^s 1 2 3 4 3)) (rhythm 1 2 1 2 3)))


(realize (namespace-provide-realizer)
  (reify-art-definitions)
  
  (rewrite-in-music
   (rewrite-in-seq (run-apl (apl:reverse *ctxt*)))
   (apply-rhythm)
   (key a 0 minor)
   (octave 4)
   (^->note)
   (delete key octave)))
