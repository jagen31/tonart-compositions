#lang racket

(require tonart tonart/organ (for-syntax syntax/parse))
(define-art-object (prompt []))


(define-art-rewriter resume-at
  (λ (stx)
    (syntax-parse stx
      [(_ prompt-name [hole-expr ...] [body-expr ...])

       (define holes (context-ref* (run-art-exprs (syntax->list #'(body-expr ...)) '()) #'hole))

       ;; fill the hole
       (define hole-exprs*
         (for/foldr ([acc '()])
                    ([h holes])
           (cons (delete-expr h) (cons (qq-art h (context hole-expr ...)) acc))))

       (define ctxt (run-art-exprs (map ensure-id-ctxt (syntax->list #`(body-expr ... #,@hole-exprs*))) '()))
       ;; find the prompt we want
       (define prompts (context-ref* ctxt #'prompt))
       (define my-prompts (filter (λ (p) (define/syntax-parse (_ p*) p) (free-identifier=? #'p* #'prompt-name)) prompts))

       (define ctxt*
         (for/fold ([acc '()])
                   ([p my-prompts])
           (define p* (remove-from-id-ctxt p #'instant))
           (define t (expr-instant p))
           (define exprs 
             (filter (λ (x) (and (or (and (expr-instant x) (<= (expr-instant x) t)) 
                                     (and (cdr (expr-interval x '(0 . #f))) (<= (expr-interval-end x) t)))
                                 
                                 (context-within? (get-id-ctxt x) (get-id-ctxt p*) '())))
                     ctxt))
           (append acc exprs)))

        (define ctxt** (append ctxt* (filter (λ (x) (and (not (expr-instant x)) (not (cdr (expr-interval x '(0 . #f)))))) ctxt)))
        (define ctxt*** (run-art-exprs (append hole-exprs* ctxt** (list #'(run-rewriters))) '()))

       (qq-art stx (context #,@ctxt*** #,@(map delete-expr hole-exprs*)))])))


(define-art motif 
 (voice@ [upper]
    (seq (^s 3 2 3 1 5 1 8 5 6 3 4 1 -1 -3))
    (rhythm 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
 (voice@ [lower]
    (i@ [4 10] (seq (^s 3 2 3 1 4))
               (rhythm 1 1 1 1 2))
    (i@ [14 30] (seq (^s 8 1 4 -3))
                (rhythm 1 1 2 2)))
  (apply-rhythm) (delete seq))

(define-art motif-prompts
  (voice@ [upper]
    (@ [(instant 11)] (prompt part))
    (@ [(instant 16)] (prompt full)))

  (voice@ [lower]
    (@ [(instant 8)] (prompt part))
    (@ [(instant 20)] (prompt full))))


(define-art motif-cont
  (i@ [0 20] (hole))
  motif
  motif-prompts
  (rewrite (^->note)))

(define-art resumed-sample
  (-- [16
       (resume-at 
           ;; tag
           full 
           ;; what to put in the hole
           [(key a 0 minor) 
            (manual II)
            (voice@ [upper] (octave 5))
            (voice@ [lower] (octave 4))]
           ;; the "continuation"
           ;; (I had to give it a name for technical reasons)
           [(name@ a motif-cont)])]
      [16
       (resume-at 
           full 
           [(key d 0 minor) 
            (manual I)
            (voice@ [upper] (octave 5))
            (voice@ [lower] (octave 4))]
           [(name@ d motif-cont)])]
      [16
       (resume-at 
           full 
           [(key g 0 major) 
            (manual II)
            (voice@ [upper] (octave 5))
            (voice@ [lower] (octave 4))]
           [(name@ g motif-cont)])]
      [16
       (resume-at 
           full 
           [(key c 0 major) 
            (manual I)
            (voice@ [upper] (octave 5))
            (voice@ [lower] (octave 4))]
           [(name@ c motif-cont)])]
      [16
       (resume-at 
           part
           [(key f 0 major) 
            (manual II)
            (voice@ [upper] (octave 5))
            (voice@ [lower] (octave 4))]
           [(name@ f motif-cont)])])
      (name@ e (i@ 72 (manual II))
      (i@ 75 (voice@ [upper] (-- [1 (note f 1 4)] [6 (note g 1 4)] [2 (note c 0 5) (note a 0 4) (note e 0 4)] [1 (note e 0 4)] [1 (note a 0 4)] [1 (note b 0 4)] [1 (note g 1 4)] [4 (note a 0 4) (note e 0 4) (note c 0 4)])))
      (i@ 72 (voice@ [lower] (-- [2 (note b 0 2)] [2 (note d 0 3)] [1 (note e 0 3)] [1 (note d 0 4)] 
                                 [1 (note e 0 4)] [1 (note b 0 3)] [1 (note c 0 4)] [1 (note g 1 3)] [1 (note a 0 3)] 
                                 [1 (note d 0 3)] [2 (note e 0 3)] [2 (note e 0 3)] [4 (note a 0 3)])))))