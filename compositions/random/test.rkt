#lang racket

(require tonart (for-syntax syntax/parse))

(define-art line
  (i@ [0 8]
    (seq (notes [c 0 4] [b 0 3] [b -1 3] [a 0 3]))
    (urhy* 2)))

(define-art melody
    (-- [8 (seq (notes [a 0 2] [c 0 3] [b 0 2] [g 1 2] [a 0 2] [g 0 2] [f 0 2] [e 0 2]))
           (rhythm 1 1 1 1 1 1/2 1/2 2)]
        [8 (seq (notes [a 0 2] [c 0 3] [b 0 2] [g 1 2] [a 0 2] [c 0 3] [d 0 3] [e 0 3]))
           (rhythm 1 1 1 1 1 1/2 1/2 2)]
        [4 (seq (notes [f 0 3] [e 0 3] [d 0 3] [a 0 2]))
           (rhythm 1 1/2 1/2 2)]
        [4 (seq (notes [f 0 3] [e 0 3] [d 0 3] [e 0 3]))
           (rhythm 1 1/2 1/2 2)]
        [8 (seq (notes [a 0 2] [g 1 2] [b -1 2] [a 0 2]))
           (rhythm 2 3/2 1/2 4)])
    (apply-rhythm))

(define-art-rewriter function->notes 
  (λ (stx)
    (qq-art stx 
      (context
        (function->image -5 5)
        (image->point-set)
        (chord->scalar-note-seq)
        (rhythm->holes)
        (fill-holes-from-points)
        (seq-ref)
        (delete point-set)))))

(define-art default-ranges
  (-- [12 (note-range [a 0 3] [a 0 5])] [12 (note-range [e 0 3] [e 0 5])]
      [8 (note-range [f 0 4] [c 0 6])]))
    
(define-art default
  (voice@ (one) melody)
  (voice@ (two)
    ;; FIXME jagen31 ideally wouldnt have to specify interval here
    (i@ [0 32]
      (function (x) (sin (* 10 x)))
      (loop 8 line (constant-structure))
      (urhy 1/8)
      default-ranges))
  (voice@ (three)
    ;; FIXME jagen31 ideally wouldnt have to specify interval here
    (i@ [0 32] (loop 8 line (constant-structure)))))

(define-art-rewriter random->notes
  (λ (stx)
    (syntax-parse stx
      [(_ qual)
       #'(context
           (voice@ (three) (expand-loop) 
             (structure-notes qual) (chord->notes/simple 3))
           (voice@ (two) (expand-loop) (structure-notes qual) (function->notes))

           (voice@ (one) (channel 3))
           (voice@ (two) (channel 1))
           (voice@ (three) (channel 2)))])))

(define-art default-notes
  default
  (random->notes [M]))


(provide (all-defined-out) (for-syntax (all-defined-out)))

#;(realize (namespace-provide-realizer)
  (reify-art-definitions))
