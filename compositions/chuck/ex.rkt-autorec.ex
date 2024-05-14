#lang racket

(require datenart (except-in tonart entity attribute) art/sequence/ravel (for-syntax syntax/parse))

(define-art st-flavian-rhythm
  (rhythm 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3))
(define-art st-flavian-^s
  (seq (^s 1 1 0 1 3 2 2 1 1 4 3 1 2 3 3 3 4 5 3 1 2 3 3 2 1 1 0 1)))

(define-art stuttgart-rhythm
  (rhythm 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2))
(define-art stuttgart-^s
  (seq (^s -2 -2 1 1 2 2 3 1 5 5 6 4 2 5 3 3 3 2 3 1 2 1 0 1 -1 -2 1 1 0 1)))

(define-art picardy-rhythm
  (rhythm 1 1 1 1 2 1 1 2 2 1 1 1 1 2 1 1 4 
          1 1 1 1 2 1 1 2 2 1 1 1 1 2 1 1 4 
          1 1 1 1 3 1 1 1 1 1 4 1 1 1 1 2 1 1 4))
(define-art picardy-^s
  (seq (^s 1 2 3 4 5 5 4 5 5 5 5 6 7 6 5 4 5 
           1 2 3 4 5 5 4 5 5 5 5 6 7 6 5 4 5 
           5 5 8 5 4 3 1 3 5 3 2 5 5 8 5 4 2 3 1)))

(define-art hymn-table-header
  (@ [(table hymn-table)]) (column-names name rhythm melody)

    #;(column-art-types number symbol (music rhythm) (music ^))
    #;(column-sql-types (integer) (varchar 20) (varchar) (varchar)))

(define-art hymn-table-sql
  (@ [(table hymn-table)]
    (row (string "St Flavian") (music st-flavian-rhythm) (music st-flavian-^s))
    (row (string "Stuttgart") (music stuttgart-rhythm) (music stuttgart-^s))
    (row (string "Picardy") (music picardy-rhythm) (music picardy-^s))))

(qr hymn-table-header hymn-table-sql)

(define-mapping-rewriter (inline-row [(: r row)])
  (λ (stx r)
    (define/syntax-parse (_ expr ...) r)
    (qq-art (remove-from-id-ctxt r #'table) (context expr ...))))

(define-mapping-rewriter (inline-music [(: m music)])
  (λ (stx m)
    (define/syntax-parse (_ expr ...) m)
    (qq-art m (context expr ...))))

(sod! 5)

;; play the music
(pmrsr
 ;; column names
 hymn-table-header

 ;; row based data
 hymn-table-sql

 (@ [(table hymn-table)]
    ;; select picardy from hymn table
    (select [rhythm melody]
      #:where (context (ref* name) (string "Picardy") (objects-equal?))
      #:into results)
    (run-select))

 ;; 
 (@ [(table results)] (inline-row))
 
 (inline-music) (apply-rhythm) (key a 0 minor) (octave 5) (^->note) (tuning 12tet) (note->tone) (volume 5))

