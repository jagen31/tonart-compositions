#lang racket

(require tonart (for-syntax syntax/parse racket/match))

(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-art-object (theka [beats]))
(define-art-object (theka-map [syms]))

(define-art dadra (theka Dha Dhin Na Dha Tin Na))
(define-art arachartal (theka Dhin Tirakita Dhi Na Tu Na Kat Ta Tirakita Dhi Na Dhi Dhi Na))

(define-art theka-volume-map
  (theka-map [Dha {(hole) (volume 10)}] [Dhin {(hole) (volume 8)}] 
             [Na {(hole) (volume 4)}] [Tin {(hole) (volume 5)}]
             [Tu {(hole) (volume 3)}] [Ta {(hole) (volume 3)}]
             [Kat {(hole) (volume 2)}] 
             [Dhi {(hole) (volume 7)}] 
             [Tirakita {(hole) (volume 4)} {(hole) (volume 2)} {(hole) (volume 3)} {(hole) (volume 2)}]))

(define-mapping-rewriter (interpret-theka [(: t theka)])
  (λ (stx t)
    (syntax-parse t
      [(_ symbol ...)
       #:with (_ tmap ...) (require-context (lookup-ctxt) t #'theka-map)

       #:do[
         (define lookup
           (for/hash ([sym (syntax->list #'(tmap ...))])
             (values (syntax->datum (car (syntax->list sym))) (cdr (syntax->list sym)))))
    
         (define symbols (syntax->list #'(symbol ...)))
         (match-define (cons start end) (expr-interval t))
         (define len (/ (- end start) (length symbols)))]


       #:with (result ...) 
         (for/list ([s symbols]) 
           (define clauses (hash-ref lookup (syntax->datum s)))
           #`[#,len (-- #,@(for/list ([clause clauses]) 
                             (define li (syntax->list clause))
                             #`[#,(/ len (length clauses)) #,@li]))])
       (println #'(result ...))
       (qq-art t (-- result ...))])))

#;(define-art sound
  theka-volume-map
  (-- [3 dadra] [3 dadra] [7 arachartal] [7 arachartal]) (interpret-theka) 
  (note a 0 4) (fill-holes note) 
  (tuning 12tet) (note->tone))


