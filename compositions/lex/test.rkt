#lang racket

(require art art/namespace tonart art/coordinate/subset (for-syntax syntax/parse racket/syntax))

(define-subset-coordinate scope scope@)

(define-art-rewriter defn-context
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...)
       (define new-scope (format-id stx "~a" (gensym)))
       (qq-art stx
         (scope@ (#,new-scope) expr ...))])))


(define-art scope-test
  (namespace
    (defn-context
      (name@ x (note a 0 4))

      (defn-context
        (name@ x (number 42)) 
        (name@ forty-two (ref x))
        
      (defn-context
        (name@ x (number 10000))
        (name@ a-lot (ref x)))
        
      (name@ note (ref x))))))

(realize (draw-realizer [800 200]) scope-test)
(realize (draw-realizer [800 200]) 
  scope-test
  (rewrite-in-namespace (resolve-ref)))
