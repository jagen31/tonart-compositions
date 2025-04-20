#lang racket

(require tonart art/coordinate/instant "dance-annotation.rkt" (prefix-in im: 2htdp/image) (for-syntax syntax/parse racket/match))

(define-art-object (arm-diagram []))
(define-art-object (facing []))
(define-art-object (full-arm-diagram []))

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

(define-mapping-rewriter (arm-diagram->full-arm-diagram [(: d arm-diagram)])
  (λ (stx d)
    (syntax-parse d
      [(_ l r)
       (define/syntax-parse (_ orient) (require-context (lookup-ctxt) d #'facing))
       (qq-art d (full-arm-diagram l r orient))])))
                                                        


(define-drawer full-arm-diagram-drawer
  (λ (e)
    (syntax-parse e
      [({~literal full-arm-diagram} l r orient)
       #'(im:scale 1/4 (make-dancer l r 'orient))]
      [_ #f])))

(register-drawer! full-arm-diagram full-arm-diagram-drawer)

(provide (all-defined-out))
