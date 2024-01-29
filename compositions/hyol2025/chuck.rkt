#lang racket

(require tonart (for-syntax syntax/parse))

(start-chucker chuck)

(define-art demo-prelude
  (voice@ (soprano) (channel 1)) 
  (voice@ (accomp) (channel 2)) 
  (voice@ (descant) (channel 1)) 
  (voice@ (bass) (channel 3))
  (volume 1))

(define-art-rewriter demo-postlude
  (lambda (stx)
    #'(context 
        (dilate 1/2) (midi->full-midi) (tone->full-tone) (d/dt))))

(define-syntax ->
  (syntax-parser
    [(_ n:number) #'(chuck (advance-time n))]))

;; realizer which sends and advances time
(define-art-realizer* play-realizer
  (λ (stx)
    #`(begin
        (chuck (send demo-prelude #,@(current-ctxt) (demo-postlude)))
        (chuck (advance-time 4)))))

;; shorthand for play realizer
(define-syntax !play
  (syntax-parser
    [(_ expr ...)
     #'(realize (play-realizer) expr ...)]))

;; realizer which sends and advances time
(define-art-realizer* send-realizer
  (λ (stx)
    #`(begin
        (chuck (send demo-prelude #,@(current-ctxt) (demo-postlude))))))

;; shorthand for send realizer
(define-syntax !send
  (syntax-parser
    [(_ expr ...)
     #'(realize (send-realizer) expr ...)]))

(provide (all-defined-out))
