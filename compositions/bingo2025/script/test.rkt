#lang racket

(require tonart (for-syntax syntax/parse))

(define-art-object (speak [text]))
(define-art-object (computer-speak-config [voice]))

(define-art-object (action [name]))
(define-art-object (computer-action-map [actions]))

(define-art-realizer script-realizer
  (λ (stx)
    (define sorted-ctxt (sort (current-ctxt) < #:key (λ (x) (expr-single-index x))))
    (define/syntax-parse (result ...)
      (for/foldr ([acc '()])
                 ([e sorted-ctxt])
        
        (syntax-parse e
          [({~literal speak} val:string)
           (define/syntax-parse (_ v:string) (require-context (current-ctxt) e #'speak-config))
           (cons #'(system (format "say -v ~a ~a" v val)) acc)]
          [({~literal sound} fn)
           (define/syntax-parse (_ actions) (require-context (current-ctxt) e #'action-map))
           #'42]
          [_ acc])))
    #'(begin result ...)))


(define-art script
  (voice@ [me] (speak "Halt, strange creatures. State you intention!"))
  (voice@ [aliens] (speak "We come in peace"))
  (voice@ [gram] (speak "HELP ME!!!"))
  (voice@ [me] (speak "LIARS! Hold on, I will save you!!"))
  (action fire-laser))

(define-art computer-config
  (voice@ [me] (computer-speak-config "fred"))
  (voice@ [aliens] (computer-speak-config "trinoids"))
  (voice@ [gram] (computer-speak-config "daria")))

(define-art-rewriter actions->sounds
  (λ (stx)
    (define actions (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'action))
    (define/syntax-parse (results ...)
      (for/list ([act actions])
        (syntax-parse act
          [(_ {~datum fire-laser}) #'(sound "laser.mp4")]
          [_ (raise-syntax-error 'actions->sounds "unrecognized action" act)])))
    #'(results ...)))
  
