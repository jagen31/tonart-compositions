#lang racket

(require tonart art/sequence/ravel (for-syntax syntax/parse racket/match))

(define-art-object (board [rows]))


(define-mapping-rewriter (board->seq [(: b board)])
  (λ (stx b)
    (syntax-parse b
      [(_ [token ...] ...)
       (qq-art b (seq (ix-- (ix-- (symbol token) ...) ...)))])))


(define-art-object (two-notes [one two]))

(define-mapping-rewriter (symbol->note [(: s symbol)])
  (λ (stx s)
    (define/syntax-parse (_ x o) (require-context (lookup-ctxt) s #'two-notes))
    (syntax-parse s
      [(_ t)
       (match (syntax-e #'t)
         ['x (qq-art s x)]
         ['o (qq-art s o)])])))



(define-art board1
  (board [x o x]
         [o x o]
         [o x o]))

(define-art board2
  (board [o o x]
         [x x o]
         [o o x]))

(define-art theme
  (-- [2 (! 0 0)] [1 (! 0 1)] [3 (! 2 2)] [2 (! 1 0)]))

(define-art my-notes (two-notes (note a 0 4) (note c 1 5)))

(define-art boards (-- [8 board1] [8 board2]))
(define-art themes (-- [8 theme] [8 theme]))