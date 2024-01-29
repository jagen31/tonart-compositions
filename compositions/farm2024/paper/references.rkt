#lang racket/base

(provide
  ~cite citet generate-bibliography
  ywiw-icfp-2002
  euterpea-2014)

(require
  scriblib/autobib)

(define-cite ~cite citet generate-bibliography
  #:style number-style)

(define ywiw-icfp-2002
  (make-bib
    #:title "Composable and Compilable Macros: You Want it When?"
    #:author (authors "Flatt, Matthew")
    #:location (proceedings-location "ACM Intl. Conf. Functional Programming" #:pages '(72 83))
    #:date 2002))

(define euterpea-2014
  (make-bib
    #:title "Euterpea"
    #:author (authors "P. Hudak")
    #:date 2014
    #:url "http://euterpea.com"))

;; Matthew Flatt. Compilable and Composable Macros, You Want it When?
;; In Proc. ACM Intl. Conf. Functional Programming, pp. 72–83, 2002.

;; P. Hudak. Euterpea, 2014. URL http://haskell.cs.yale.edu/
;; euterpea/.