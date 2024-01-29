#lang at-exp racket

(require art (for-syntax racket/base syntax/parse syntax/parse))

(define-art-object (racket []))

(define-art sample-text
  (racket
    @string-append{
      Racket is fun!
    }))

(define-art-realizer racket-realizer
  (λ (stx)
    (define racket-exprs (context-ref* (current-ctxt) #'racket))
    (define/syntax-parse ((_ expr) ...) racket-exprs)
    #`(begin expr ...)))

(realize (racket-realizer) sample-text)

(define-art-object (theme [name]))

(define-art themes
  (theme programming-with-syntax-objects)
  (theme specializing-using-additional-context)
  (theme interpretation-in-music)
  (theme using-hyol-techniques-in-industry))

(define-art abstract
  (ix--
    (sentence "This lecture will take us through the design and implementation of my personal markup language called art")
    (sentence "In a similar way to how people constantly write DSLs in YAML or formerly, XML, Art is made for producing DSLs") 
    (sentence "Art is specifically designed for making DSLs in performance domains")

    (sentence "Two art languages I use frequently are my music language (Tonart), and a script language for writing/performing plays")
    (sentence "In addition to showing you how these languages work, I will show how they compose together (surely a play can have music!)")
    (sentence "I will also demonstrate building a simple shopping list language from scratch")
    (sentence "For fun, we will find a way to compose boring shopping lists into the wonderful world of music and drama")

    (sentence "One important thing to know about Art is that it is implemented using racket syntax objects")
    (sentence "Syntax objects provide a bunch of features that have to be hand-implemented in DSLs written in other languages")
    (sentence "These features tend to be the same or similar for every DSL, but can dominate the implementation effort and maintenance burden of a DSL")
    (sentence "If you are doing a syntax-y task, syntax object programming may be the right choice")
    (sentence "Once you understand the features of syntax objects, you can to try to emulate some of them when making DSLs in other languages")

    (sentence "An ")
    (sentence ""))))