#lang typed/racket

(struct lit [(n : Number)])
(struct fun [(arg : Symbol) (body : Exp)])
(struct app [(head : Exp) (arg : Exp)])
(struct ref [(name : Symbol)])

(define-type Exp (U lit fun app ref))

(struct numV [(n : Number)] #:transparent)
(struct cloV [(arg : Symbol) (body : Exp) (env : Env)] #:transparent)

(define-type Value (U numV cloV))

(define-type Env (Listof (Pairof Symbol Value)))

(: eval (Exp Env -> Value))
(define (eval e env)
  (match e
    [(lit n) (numV n)]
    [(ref n) (match (assoc n env)
               [(cons x v) v]
               [else (error 'eval "unbound variable")])]
    [(fun arg body) (cloV arg body env)]
    [(app head arg)
     (match (eval head env)
       [(cloV arg* body env)
        (eval body (cons (cons arg* (eval arg env)) env))]
       [v (error 'eval "expected a function, got ~a" v)])]))
