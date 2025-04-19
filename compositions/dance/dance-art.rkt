#lang racket

(require tonart art/coordinate/instant "dance-annotation.rkt" (prefix-in im: 2htdp/image) (for-syntax syntax/parse racket/match))

(define-art-object (arm-diagram []))
(define-art-object (facing []))

(define-art-rewriter arm-diagrams
  (λ (stx)
    (syntax-parse stx
      [(_ [l:number r:number] ...)
       (qq-art stx (ix-- (arm-diagram l r) ...))])))

(define-art-rewriter facings
  (λ (stx)
    (syntax-parse stx
      [(_ f* ...)
       (define/syntax-parse (face ...)
         (for/list ([f (syntax->datum #'(f* ...))])
           (match f ['t 'towards] ['a 'away] ['l 'left] ['r 'right])))
       (qq-art stx (ix-- (facing face) ...))])))


(define-drawer arm-diagram-drawer
  (λ (e)
    (define/syntax-parse (_ direction) (require-context (lookup-ctxt) e #'facing))
    (syntax-parse e
      [({~literal arm-diagram} l r)
       #'(im:scale 1/3 (make-dancer l r 'direction))]
      [_ #f])))

(register-drawer! arm-diagram arm-diagram-drawer)

(provide (all-defined-out))
