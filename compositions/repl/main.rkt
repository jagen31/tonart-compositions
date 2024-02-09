#lang racket

(require art tonart tonart/common-practice (for-syntax data/gvector syntax/parse racket/string))

(define-for-syntax the-exprs (gvector))
(define the-output (box (silence 1)))

(define looping (box #f))
(define loop-length (box 8))

(define (do-loop)
  (thread (λ ()
    (let loop ()
      (play (unbox the-output))
      (sleep (unbox loop-length))
      (when (unbox looping) (loop))))))

(define-syntax (compile stx) #`(set-box! the-output (mrsr #,@(gvector->list the-exprs))))

(define-art-object (comment []))

(define-syntax #%top-interaction
  (syntax-parser
    [(_ {~datum add} expr)
     (gvector-add! the-exprs #'expr)
     #'(compile)]
    [(_ {~datum add} n:number expr)
     (gvector-insert! the-exprs (syntax-e #'n) #'expr)
     #'(compile)]
    [(_ {~datum set} n:number expr)
     (gvector-set! the-exprs (syntax-e #'n) #'expr)
     #'(compile)]
    [(_ {~datum remove} i:number)
     (gvector-remove! the-exprs (syntax-e #'i))
     #'(compile)]
    [(_ {~datum play})
     #`(pmrsr #,@(gvector->list the-exprs))]
    [(_ {~datum show})
     (displayln
       (string-join 
         (for/list ([expr (gvector->list the-exprs)] [i (in-naturals)])
           (format "~a: ~a" i (syntax->datum expr))) "\n"))
     #'(void)]
    [(_ {~datum loop})
     #'(begin (set-box! looping #t) (do-loop))]
    [(_ {~datum stop-loop})
     #'(set-box! looping #f)]
    [(_ {~datum loop-length} n:number)
     #'(set-box! loop-length n)]
    [(_ {~datum comment} n:number)
     #:with expr (gvector-ref the-exprs (syntax-e #'n))
     (gvector-set! the-exprs (syntax-e #'n) #'(comment expr))
     #'(compile)]
    [(_ {~datum uncomment} n:number)
     (define expr 
       (syntax-parse (gvector-ref the-exprs (syntax-e #'n))
         [({~literal comment} expr) #'expr]))
     (gvector-set! the-exprs (syntax-e #'n) expr)
     #'(compile)]
    [(_ {~datum racket} expr ...)
     #'(begin expr ...)]))

(define-art accomp (seq (^s 1 -2 2 -2 3 -2 4 2)) (i@ [0 4] (urhy* 0.5)))

(define-art accomp2 (seq (chords [a 0 M] [d 0 M] [e 0 M] [c 1 m] [b 0 m] [f 1 m] [e 0 M] [e 0 M 7])) 
                    (i@ [0 8] (urhy* 1)))