#lang racket

(require tonart (prefix-in ta: (only-in tonart staff-realizer)) (for-syntax syntax/parse racket/math))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-art-realizer staff-realizer
  (λ (stx)
    (syntax-parse stx
      [(_ [w h])
       #:with soprano (datum->syntax stx 'soprano)
       #:with countermelody (datum->syntax stx 'countermelody)
       #:with accomp (datum->syntax stx 'accomp)
       #`(realize (ta:staff-realizer [w h] 
                    {[soprano treble] [countermelody treble] [accomp bass]})
           #,@(current-ctxt))])))
                  
(define-art-rewriter chord->notes
  (λ (stx)
    (syntax-parse stx
      [(_ n) #'(chord->notes/simple n)])))


(define-art-rewriter function->notes
   (λ (stx)
     (syntax-parse stx
       [(_ [l h] [nl hl])
        (qq-art stx
          (context
            (function->image l h)
            (image->point-set)
            (rhythm->holes)
            (chord->scalar-note-seq nl hl)
            (fill-holes-from-points)
            (delete point-set)
            (seq-ref)))])))