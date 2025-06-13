#lang racket

(require tonart (for-syntax syntax/parse))
(provide (all-defined-out))

(define-art-object (location []))

(define-art my-music
  (voice@ [one] picardy-^s picardy-rhythm (apply-rhythm) (dilate 1/2) (octave 5))
  (voice@ [two]
          (seq (music happy-birthday-^s happy-birthday-rhythm (apply-rhythm))
               (music happy-birthday-^s happy-birthday-rhythm (apply-rhythm)))
          (inline-music-seq)
          ;; FIXME jagen31 pickup note HACK
          (i@ [0 1] (location kitchen))
          (octave 4)
          (translate -1))

  #;(voice@ [one]
          nettleton-^s nettleton-rhythm (apply-rhythm)
          (i@ [0 1] (location kitchen))
          (octave 5)
          (translate -1))
  #;(voice@ [two] aus-tiefer-not-^s aus-tiefer-not-rhythm (apply-rhythm) (octave 4)))

(define-art rooms
  (i@ [0 96]
      (loop 12
            (-- [4 (location kitchen)]
                [4 (location livingroom)]
                [4 (location dining-room)])))
  (expand-loop))

(define-art rooms2
  (voice@ [one]
          (-- [8 (location kitchen)]
              [8 (location livingroom)]
              [8 (location dining-room)]
              [8 (location livingroom)]
              [8 (location kitchen)]
              [8 (location livingroom)]
              [8 (location dining-room)]
              [8 (location livingroom)]))
  (voice@ [two]
          (-- [4 (location livingroom)]
              [8 (location kitchen)]
              [4 (location livingroom)]
              [4 (location dining-room)]
              [8 (location livingroom)]
              [4 (location dining-room)]
              [4 (location livingroom)]
              [8 (location kitchen)]
              [4 (location livingroom)]
              [4 (location dining-room)]
              [8 (location livingroom)]
              [4 (location dining-room)])))

(define-art-rewriter location->instrument
  (λ (stx)
    (define locations (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'location))
    (define instruments
      (for/list ([loc locations])
        (syntax-parse loc
          #:datum-literals [kitchen livingroom dining-room]
          [(_ kitchen) (qq-art loc (instrument Clarinet))]
          [(_ livingroom) (qq-art loc (instrument Flute))]
          [(_ dining-room) (qq-art loc (instrument |Yamaha Grand Piano|))])))
    #`(context #,@(map delete-expr locations) #,@instruments)))

(define-art-rewriter location->channel
  (λ (stx)
    (define locations (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'location))
    (define channels
      (for/list ([loc locations])
        (syntax-parse loc
          #:datum-literals [kitchen livingroom dining-room]
          [(_ kitchen) (qq-art loc (channel 7)) #;(qq-art loc (channel 4))]
          [(_ livingroom) (qq-art loc (channel 1))]
          [(_ dining-room) (qq-art loc (channel 4))])))
    #`(context #,@(map delete-expr locations) #,@channels)))

(define-art-rewriter correct-sound
  (λ (stx)
    (define/syntax-parse (_ place time:number) stx)
    ;; get everything happening in the livingroom!
    (define livingroom-exprs
      (filter (λ (expr)
                (define/syntax-parse (head more ...) expr)
                (define/syntax-parse (_ loc) (or (get-context (lookup-ctxt) expr #'location) #'(blah nowhere-special)))
                (or (eq? (syntax->datum #'loc) (syntax->datum #'place))
                    (and (free-identifier=? #'head #'location) (equal? (syntax->datum #'(more ...)) (syntax->datum #'(place))))))
              (current-ctxt)))
    (define livingroom-exprs* (run-art-exprs (append livingroom-exprs (list #'(translate time))) '()))
    #`(context #,@(map delete-expr livingroom-exprs) #,@livingroom-exprs*)))

(define-art t1 (^s 1 2 3 2))
(define-art t2 (^s 1 -3 -2 -1 0))

(define-art lr (location livingroom))
(define-art kitch (location kitchen))
(define-art din (location dining-room))

#;(start-chucker chuck)

(define-syntax-rule (!! exprs ...)
  (chuck (send exprs ... (location->channel) (midi->full-midi) (d/dt))))

(define-syntax-rule (-> n)
  (chuck (advance-time n)))
