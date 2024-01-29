#lang racket

(require art art/coordinate/name (for-syntax syntax/parse))

(define-art-object (a-file []))
(define-art-embedding (directory [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (context expr ...)))])))

(define-art-object (cursor []))

(define-art-rewriter dir
  (λ (stx)
    (syntax-parse stx
      [(_ name stuff ...)
       (qq-art stx (name@ name stuff ...))])))

(define-art-rewriter file
  (λ (stx)
    (syntax-parse stx
      [(_ name)
       (qq-art stx (name@ name (a-file)))])))

(define-art my-fs
  (dir home
    (file test.txt)
    (dir documents
      (file a_document.jpg))
      (dir random
           (file bar.wav))))

(define-art-object (ls []))

(define-art it
  (name@ file-system (namespace my-fs))
  (ls))

(define test%
  (class object%
    (super-new)
    (define/public (hello-world) (print "hello world"))))

(define test (new test%))
(send test asdf)