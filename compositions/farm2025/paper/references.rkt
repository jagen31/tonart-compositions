#lang racket/base

(provide
  ~cite citet generate-bibliography
  ywiw-icfp-2002
  tonart-farm-2024
  musicxml-2001
  visual-2020
  sketch-2019)

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

(define tonart-farm-2024
  (make-bib
    #:title "Demo: Composable Compositions with Tonart"
    #:author (authors "Gentner, Jared")
    #:location (proceedings-location 
     "12th ACM SIGPLAN International Workshop on Functional Art, Music, Modelling, and Design" 
     #:pages '(42 44))
    #:date 2024
    #:url "https://doi.org/10.1145/3677996.3678294"))

(define musicxml-2001
  (make-bib
    #:title "MusicXML: An Internet-Friendly Format for Notation Interchange"
    #:author (authors "Good, Michael")
    #:location (proceedings-location "Proceedings of XML")
    #:date 2001
    #:url "https://www.musicxml.com/for-developers/"))

(define sketch-2019
  (make-bib
    #:title "Sketch-n-Sketch: Output-Directed Programming for SVG"
    #:author (authors "Hempel, Brian" "Lubin, Justin" "Chugh, Ravi")
    #:location (proceedings-location "32nd Annual ACM Symposium on User Interface Software and Technology"  
                                     #:pages '(281 292))
    #:date 2019
    #:url "https://doi.org/10.1145/3332165.3347925"))

(define visual-2020
  (make-bib
    #:title "Adding interactive visual syntax to textual code"
    #:author (authors "Andersen, Leif" "Ballantyne, Michael" "Felleisen, Matthias")
    #:location (proceedings-location "ACM on Programming Languages" 
                                     #:pages '(1 28))
    #:date 2020
    #:url "https://doi.org/10.1145/3428290"))

;; Matthew Flatt. Compilable and Composable Macros, You Want it When?
;; In Proc. ACM Intl. Conf. Functional Programming, pp. 72–83, 2002.

;; P. Hudak. Euterpea, 2014. URL http://haskell.cs.yale.edu/
;; euterpea/.