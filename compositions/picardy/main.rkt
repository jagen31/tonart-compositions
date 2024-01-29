#lang racket

(require art tonart "picardy.rkt" (for-syntax syntax/parse racket/dict syntax/id-table))

(define-art the-piece-rsound
  the-piece
  (x-picardy-tune)
  (metric-interval->interval)

  (voice@ (one) (instrument Clarinet)) 
  (voice@ (two) (instrument |Yamaha Grand Piano|)) 
  (voice@ (three) (instrument Clarinet))
  (voice@ (four) (instrument Clarinet))
  (voice@ (five) (instrument Clarinet))

  (key d 0 minor) (^->note) (tuning 12tet) (note->tone) 
  (volume 5)

  (tempo 120) (apply-tempo))

(define-art the-piece-lsampler
  the-piece
  (x-picardy-tune)
  (metric-interval->interval)

  (voice@ (one) (instrument flute)) 
  (voice@ (two) (instrument viola)) 
  (voice@ (three) (instrument pedal))
  (voice@ (four) (instrument viola))
  (voice@ (five) (instrument viola))

  (instrument-map [flute . 001/005_Flute_harmonique] [viola . 000/020_Viola_8_Flute_4]
                  [pedal . |000/112_Prin.(ped.)16+8|])

  (key d 0 minor) (^->note) (note->midi) (midi->full-midi)
  (tempo 120) (apply-tempo) (d/dt))


#;(define-art the-piece-mxml
  the-piece
  (x-picardy-tune)
  (metric-interval->interval) 
  (key d 0 minor) (^->note) 

  (time-sig 4 4)
  (enclose-in-measures [note])
  (rewrite-in-seq 
    (insert-rests)
    (measure->mxml-measure) #:capture [time-sig]))

(define-art-rewriter chuck-postlude
  (lambda (stx)
    #'(context 
        (x-picardy-tune) 
        (sound-map) 
        (^->note) (note->midi)  (volume 5) 
        (voice@ (one) (channel 1)) (voice@ (two) (channel 2)) 
        (voice@ (three) (channel 3)) 
        (voice@ (four) (channel 1)) (voice@ (five) (channel 1)) 
        (midi->full-midi) (tempo 120) (apply-tempo) (d/dt))))

#;(realize (draw-realizer [800 200]) (music the-piece))

#;(rs-write (realize (music-rsound-realizer) the-piece-rsound) "picardy.wav")

#;(displayln (realize (linuxsampler-realizer) the-piece-lsampler)
           (open-output-file "picardy.cpp" #:exists 'replace))

#;(write-xml (realize (unload-musicxml) the-piece-mxml)
           (open-output-file "picardy.musicxml" #:exists 'replace))

(define-art-object (funart-fun [args bodies]))
(define-art-object (funart-app [id args]))
(define-art-object (funart-var [id]))

(define-art-rewriter eval-funart
  (λ (stx)
    (define apps (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'funart-app))
    (define result (for/fold ([bodies* '()] #:result (reverse bodies*)) ; where the bodies are buried
              ([app apps])

        (define defns 
          (context-ref*/surrounding (lookup-ctxt) (get-id-ctxt app) #'funart-fun))


        (define defn-lookup
          (for/fold ([m (make-immutable-free-id-table)]) 
                    ([defn defns])
            (syntax-parse defn
              [({~literal funart-fun} name [arg ...] body ...)
               (free-id-table-set m #'name (list (syntax->list #'(arg ...)) (syntax->list #'(body ...))))])))

      (syntax-parse app
        [({~literal funart-app} the-id arg ...)
         (define defn (free-id-table-ref defn-lookup #'the-id (λ () (error "undefined funart-fun" #'the-id))))
         (define arg-names (car defn))
         (define bodies (cadr defn))
         (define bodies-subst
           (for/fold ([bodies-evald (run-art-exprs bodies '() (lookup-ctxt))])
                     ([arg-name arg-names] [val (syntax->list #'(arg ...))])
               (for/list ([b bodies-evald])
                 (syntax-parse b
                   [({~literal funart-var} my-id)
                    (if (free-identifier=? #'my-id arg-name) (qq-art app #,val) b)]
                   [e (qq-art app e)]))))
         (append (reverse (cons (delete-expr app) bodies-subst)) bodies*)])))
         (println result)
         #`(context #,@result)))