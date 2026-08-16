#lang racket

;; A little resource table, modeled on hymnal.rkt.  A "resource" is the
;; general version of a hymn: it has a name, an optional author, and
;; contents -- exactly what you want when you'd rather *reference* a
;; thing by name in the program than print the whole thing inline (a
;; prelude score, a reading, a poem, …).  The full contents live in the
;; table; the program mentions the resource and pulls in whichever form
;; it wants:
;;
;;   (define-art order
;;     resources
;;     (resource prelude)
;;     (resource->title))         ; just the "Name — Author" line
;;
;;   ;; …and elsewhere (say, an appendix) the whole thing:
;;   (define-art appendix
;;     resources
;;     (resource prelude)
;;     (resource->contents))
;;
;; Add a resource by adding a `row`; nothing else needs to change.

(require (except-in tonart direction transpose-octave)
         (except-in "../../scribble/scribble.rkt" insert)
         "prelude.rkt"
         "talk-deck.rkt"
         ;; `select` collides with tonart's and `row` with the seq
         ;; combinator, so take only the table vocabulary used here.
         (only-in datenart table column-names column-art-types
                  column-sql-types row)
         (for-syntax syntax/parse racket/list racket/string
                     (only-in art/private/core get-id-ctxt context-ref)))

(provide resources resource-header resource-rows
         resource resource->title resource->contents)

;; name is the handle you reference (a symbol); author is optional (an
;; empty string prints no attribution); contents is the resource itself,
;; held in a `(music …)` embedding — a score for the common case, but any
;; art that can sit in a music block works.
(define-art resource-header
  (@ [(table resources)]
     (column-names name title author contents)
     (column-art-types symbol string string (music note))
     (column-sql-types (varchar 48) (varchar 48) (varchar 48) (varchar))))

;; Example: the Prelude, so it need not be engraved in the program body.
;; `author` is left empty to show the optional case.
(define-art resource-rows
  (@ [(table resources)]
     (row (symbol prelude)
          (string "Herzlich Willkommen zu meiner Art-demo")
          (string "Jared Gentner")
          (music theme (dilate 1)))
     (row (symbol slides)
          (string "The Tasteful Embedded Slideshow")
          (string "Jared Gentner")
          talk-deck)))

(define-art resources resource-header resource-rows)

;; -------------------------------------------------------------------
;; resource, and the two ways it compiles
;; -------------------------------------------------------------------

;; What you write in a program: `(resource prelude)` and nothing else --
;; the table knows the author and the contents.
(define-art-object (resource [name]))

(begin-for-syntax

  ;; Pull the table's rows out of the surrounding context.  Rows carry
  ;; the `(table resources)` coordinate, so a `resource` sitting anywhere
  ;; in the program still finds them as long as `resources` is in scope.
  (define (resource-rows-in ctxt)
    (filter (λ (e) (syntax-parse e
                     [({~literal row} _ ...)
                      (syntax-parse (context-ref (get-id-ctxt e) #'table)
                        [({~datum table} {~datum resources}) #t]
                        [_ #f])]
                     [_ #f]))
            ctxt))

  ;; Column order as declared in `resource-header`.
  (define resource-columns '(name author title contents))

  (define (row-cell r col)
    (define ix (index-of resource-columns col))
    (and ix (list-ref (cdr (syntax->list r)) ix)))

  (define (cell-value c)
    (syntax-parse c
      [({~literal symbol} s) (syntax-e #'s)]
      [({~literal string} s) (syntax-e #'s)]
      [_ #f]))

  ;; Find the row for a name, or fail with a message that lists the names
  ;; that *do* exist -- a typo in the name is the likeliest mistake.
  (define (lookup-resource stx n)
    (define rows (resource-rows-in (lookup-ctxt)))
    (or (findf (λ (r) (eq? n (cell-value (row-cell r 'name)))) rows)
        (raise-syntax-error
         'resource
         (format "no resource ~a (have: ~a)"
                 n (string-join
                    (for/list ([r (in-list rows)])
                      (format "~a" (cell-value (row-cell r 'name))))
                    ", "))
         stx)))

  ;; "Name" or "Name — Author"; an empty author drops the dash.
  (define (title-string r)
    (define name (cell-value (row-cell r 'title)))
    (define author (cell-value (row-cell r 'author)))
    (if (and author (not (string=? author "")))
        (format "~a — ~a" author name)
        (format "~a" name))))

;; Display form: the name (+ author) line a program shows in place of
;; the contents.
(define-mapping-rewriter (resource->title [(: r resource)])
  (λ (stx res)
    (syntax-parse res
      [(_ n:id)
       (define row (lookup-resource res (syntax-e #'n)))
       (qq-art res (text #,(title-string row)))])))

;; Full form: the title line with the contents spliced underneath.
(define-mapping-rewriter (resource->contents [(: r resource)])
  (λ (stx res)
    (syntax-parse res
      [(_ n:id)
       (define row (lookup-resource res (syntax-e #'n)))
       (syntax-parse (row-cell row 'contents)
         [(expr ...)
          (qq-art res
            (ix--
             (text #,(title-string row))
             (expr ...)))])])))
