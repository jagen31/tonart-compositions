#lang racket

(require tonart (prefix-in ta: (only-in tonart staff-realizer)) (for-syntax syntax/parse racket/math racket/set))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-art-realizer staff-realizer
  (λ (stx)
    (syntax-parse stx
      [(_)
       #:with soprano (datum->syntax stx 'soprano)
       #:with countermelody (datum->syntax stx 'countermelody)
       #:with accomp (datum->syntax stx 'accomp)
       #:do [(define num-voices (length (set->list (set (map (λ (x) (syntax->datum (car (expr-voice x)))) (current-ctxt))))))]
       #`(realize (ta:staff-realizer [300 #,(* 150 num-voices)] 
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
            (note-range nl hl)
            (chord->scalar-note-seq)
            (fill-holes-from-points)
            (delete point-set)
            (seq-ref)))])))