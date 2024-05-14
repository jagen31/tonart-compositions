#lang racket/base

(provide
  ~cite citet generate-bibliography
  fbf-icfp-2009)

(require
  scriblib/autobib)

(define-cite ~cite citet generate-bibliography
  #:style author+date-square-bracket-style)

(define icfp "ICFP")

(define fbf-icfp-2009
  (make-bib
    #:title "Scribble: Closing the Book on Ad Hoc Documentation Tools"
    #:author (authors "Matthew Flatt" "Eli Barzilay" "Robert Bruce Findler")
    #:location (proceedings-location icfp #:pages '(109 120))
    #:date 2017))