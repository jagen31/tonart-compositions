#lang racket

(require tonart (for-syntax syntax/parse racket/match) "organ-config.rkt"
         (prefix-in ss: slideshow) slideshow/slides-to-picts)
(provide (all-defined-out) (for-syntax (all-defined-out)))

(start-chucker chuck chuck-realizer)

(define-art-object (rkt []))
(define-art-object (comment []))

(define-art-realizer assemble-racket-timed
  (λ (stx)
    (define result 
      (for/fold ([acc '()] #:result (reverse acc))
                ([expr (current-ctxt)])
        (syntax-parse expr
          [({~literal rkt} code ...)
           (println (un-@ expr))
           (define t 
             (or (expr-instant expr) (car (expr-interval expr '(#f)))))
           (if t
               (cons #`(thread (λ () (begin (sleep #,t) (eval-syntax #'(let () code ...))))) acc)
               acc)]
          [_ acc])))
    (println #`(begin #,@result))
    #`(begin #,@result)))

(define-art-object (go-to-slide [n]))

(define-mapping-rewriter (slide->rkt [(: sl go-to-slide)])
  (λ (stx sl)
    (syntax-parse sl
      [(_ n) (qq-art sl (rkt (set-slide! n)))])))

(define-syntax (!perform stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(realize (assemble-racket-timed) expr ... (slide->rkt))]))

(define-art sm (sound-map [snare . "sounds/snare.wav"] [kick . "sounds/kick.wav"] [cymbal . "sounds/cymbal.wav"]))

(define-art pre man-conf stop-conf piston-conf sm)
(define-art-rewriter post
  (λ (stx)
    (syntax-parse stx
      [_
       (qq-art stx
         (context
            (registration->full-midi 8)
            (registration+->full-midi 8)
            (manual->channel)
            (midi->full-midi)
            (tone->full-tone)
            (d/dt)))])))


(define-for-syntax state
  (box (run-art-expr
    #'(context
        (key e 0 minor)
        (mode aeolian)
        (tuning 12tet)
        (volume 0.1)) '())))
(define-for-syntax post-state 
  (box (run-art-expr 
    #'(context
        #;(voice@ [one] (manual I))
        #;(voice@ [two] (manual I))
        (voice@ [pedal] (manual P))
        (voice@ [upper] (manual I) (octave 4))
        (voice@ [lower] (manual II) (octave 3))
        #;(manual I)
        (quoted-rewrite (run-invert))
        (quoted-rewrite (mode->transpose))
        (quoted-rewrite (run-transpose-diatonic))

        (quoted-rewrite (^->^o))
        (quoted-rewrite (^o->note))
        #;(quoted-rewrite (chord^->chord))
        #;(quoted-rewrite (chord->notes/simple 4))
        (quoted-rewrite (delete seq))
        (ix@ 2 (name@ note-compiler (quoted-rewrite (note->midi)))))
      '())))
(define-for-syntax state-dirty (box #t))


(define the-slides (box (get-slides-as-picts "slides.rkt" 1100 800 #f)))

(define current-slide (box 0))
(define the-slide (box (car (unbox the-slides))))

(define (set-slide! ix)
  (set-box! current-slide ix)
  (set-box! the-slide (list-ref (unbox the-slides) ix)))

(define (refresh-slide!)
  (set-box! the-slide (list-ref (unbox the-slides) (unbox current-slide))))


(define-for-syntax (get-post-state)
  (sort (unbox post-state) < #:key expr-single-index))


(define-syntax-rule (-> n)
  (chuck (advance-time n)))

(define-syntax-rule (-to n) (-> n))

(define-syntax (!! stx)
  (syntax-parse stx
    [(_ exprs ...)
     #`(begin
         (chuck (send pre #,@(unbox state) exprs ...  #,@(get-post-state) (run-rewriters) (post))))]))

(define-syntax (!<- stx)
  (syntax-parse stx
    [(_ expr ...)
     (set-box! state-dirty (box #t))
     #'(begin
         (define-syntax _ (set-box! state (run-art-exprs (syntax->list #'(expr ...)) (unbox state))))
         (define-syntax __ (set-box! state-dirty (box #t))))]))

(define-syntax-rule (!ctxt expr ...) (!<- expr ...))

(define-syntax (!-> stx)
  (syntax-parse stx
    [(_ expr ...)
     (set-box! post-state (run-art-exprs (append (get-post-state) (syntax->list #'(expr ...))) '()))
     (set-box! state-dirty (box #t))
     #'(void)]))

(define-syntax-rule (!post expr ...) (!-> expr ...))

(define-mapping-rewriter (M-to-m [(: k key)])
  (λ (stx k)
    (syntax-parse k
      [(key p a m)
       (define/syntax-parse m*
         (match (syntax-e #'m) [(or 'M 'major) 'minor] [(or 'm 'minor) 'major]))
       (qq-art k (key p a m*))])))


(define-mapping-rewriter (midi-to-tone [(: r quoted-rewrite)])
  (λ (stx r)
    (syntax-parse r
      [(_ ({~literal note->tone})) (qq-art r (quoted-rewrite (note->midi)))]
      [(_ ({~literal note->midi})) (qq-art r (quoted-rewrite (note->tone)))]
      [_ r])))

(define-art-rewriter ^o-to-note
  (λ (stx)
    (define/syntax-parse (result ...) (run-art-expr #'(^o->note) (current-ctxt) (lookup-ctxt)))
    #'(replace-full-context result ...)))
    
(define-art-rewriter $
  (syntax-parser
    [(_ expr ...) (qq-art this-syntax (voice@ [upper] (reset) (set expr ...)))]))

(define-art-rewriter %
  (syntax-parser
    [(_ expr ...) (qq-art this-syntax (voice@ [lower] (reset) (set expr ...)))]))

(define-mapping-rewriter (transpose-transpose-diatonic [(: td transpose-diatonic)])
  (λ (stx td)
    (define/syntax-parse (_ n) td)
    (define/syntax-parse (_ n*) stx)
    (qq-art td (transpose-diatonic #,(modulo (+ (syntax-e #'n) (syntax-e #'n*)) 7)))))

(define-art-rewriter toggle-invert
  (λ (stx)
    (define inverted (context-ref* (lookup-ctxt) #'invert))
    (if (null? inverted)
        #'(context  (voice@ [upper] (invert 1 0 5))
                    (voice@ [lower] (invert 1 0 4))
                    (voice@ [pedal] (invert 1 0 4)))
        #`(context #,@(map delete-expr inverted)))))
