#lang racket/base

;; struct
(struct tone [freq])
(define tone-struct (tone 440))
;; list
(define tone-list '(tone 440))
;; syntax object
(define tone-syntax #'(tone 440))



;; de-struct
(require racket/match)
(match tone-struct
  [(tone freq) freq])

;; de-list
(match tone-list
  [`(tone ,freq) freq])

;; de-syntax
(require syntax/parse)
(syntax-parse tone-syntax
  [({~datum tone} freq:number) (syntax-e #'freq)])

#;(syntax-parse tone-syntax
  [({~datum tone} freq:number) (raise-syntax-error 'oops "an error" #'freq)])


(require rsound)

(define (seconds->frames s)
  (* (default-sample-rate) s))

(define (interpret expr)
  ;; rsound play stream
  (define ps (make-pstream))
  (syntax-parse expr
    [({~datum tone} freq:number)
     (pstream-play ps (make-tone (syntax-e #'freq) 1 (seconds->frames 2)))]))

(define (interpret2 exprs)
  ;; rsound play stream
  (define ps (make-pstream))
  (for ([expr exprs])
    (syntax-parse expr
      [({~datum tone} freq:number)
       (pstream-play ps (make-tone (syntax-e #'freq) 0.1 (seconds->frames 2)))])))

(define note-syntax #'(note a))

(define (note->tone n)
  (define rp 264)
  (syntax-parse n
    [({~datum note} pitch)
     (quasisyntax/loc
        n
        (tone #,(match (syntax-e #'pitch)
                 ['c rp]
                 ['d (* rp 9/8)]
                 ['e (* rp 5/4)]
                 ['f (* rp 4/3)]
                 ['g (* rp 3/2)]
                 ['a (* rp 5/3)]
                 ['b (* rp 15/8)]
                 [_ (raise-syntax-error 'note->tone "invalid pitch symbol" #'pitch)])))]))


(define (lookup ctxt head)
  (findf (λ (x) (eq? (syntax-e head) (syntax-e (car (syntax->list x))))) ctxt))

(define (require-context ctxt head)
  (define result (lookup ctxt head))
  (or result
    (raise-syntax-error 'require-context (format "no ~s in context" (syntax-e head)))))

(define (interpret3 exprs ctxt)
  ;; rsound play stream
  (define ps (make-pstream))
  (for ([expr exprs])
    (syntax-parse expr
      [({~datum tone} freq:number)
       (define/syntax-parse ({~datum volume} vol:number) (require-context ctxt #'volume))
       (unless (<= (syntax-e #'vol) 10)
         (raise-syntax-error 'interpret3 "too loud!!! volume must be lte 10" #'vol))
       (pstream-play ps (make-tone (syntax-e #'freq) (/ (syntax-e #'vol) 100) (seconds->frames 2)))])))

(define (just-intone p rp)
  (match p
    ['c rp]
    ['d (* rp 9/8)]
    ['e (* rp 5/4)]
    ['f (* rp 4/3)]
    ['g (* rp 3/2)]
    ['a (* rp 5/3)]
    ['b (* rp 15/8)]
    [_ (raise-syntax-error 'note->tone "invalid pitch symbol" #'pitch)]))

(define (equal-temper p rp)
  (match p
    ['c rp]
    ['d (* rp (expt 2 2/12))]
    ['e (* rp (expt 2 4/12))]
    ['f (* rp (expt 2 5/12))]
    ['g (* rp (expt 2 7/12))]
    ['a (* rp (expt 2 9/12))]
    ['b (* rp (expt 2 11/12))]))

(define (note->tone2 n ctxt)
  (define rp 264)
  (syntax-parse n
    [({~datum note} pitch)
     (define/syntax-parse ({~datum tuning} tuning-name) (require-context ctxt #'tuning))
     (define tuner (match (syntax-e #'tuning-name) ['12tet equal-temper] ['just just-intone]))
     (quasisyntax/loc
        n
        (tone #,(tuner (syntax-e #'pitch) rp)))]))

(define (rewrite f exprs ctxt)
  (map (λ (x) (f x ctxt)) exprs))

;; Fail 1
;; ({~datum note} pitch) -> ({~datum note} pitch start end)
;; ({~datum tone} freq) -> ({~datum tone} freq start end)
;; Ew


(require racket/dict)
(define (@ h expr)
  (for/fold ([acc expr])
            ([(k v) (in-dict h)])
    (syntax-property acc k v)))

(define (interpret4 exprs ctxt)
  ;; rsound play stream
  (define ps (make-pstream))
  (for ([expr exprs])
    (syntax-parse expr
      [({~datum tone} freq:number)
       (define/syntax-parse ({~datum volume} vol:number) (require-context ctxt #'volume))
       (define the-start (syntax-property expr 'start))
       (define the-end (syntax-property expr 'end))
       (unless (<= (syntax-e #'vol) 10)
         (raise-syntax-error 'interpret3 "too loud!!! volume must be lte 10" #'vol))
       (pstream-queue ps (+ (pstream-current-frame ps) (seconds->frames the-start)
         (make-tone (syntax-e #'freq) (/ (syntax-e #'vol) 100) (seconds->frames the-end))))])))

;; ^^^ this was fail 2

(define id-ctxt-property (gensym 'id-context))

(define (get-id-ctxt expr)
  (syntax-property expr id-ctxt-property)) 
(define (set-id-ctxt expr ctxt)
  (syntax-property expr id-ctxt-property ctxt))

(define (@2 ctxt . exprs) 
  (for/list ([e exprs]) 
    (set-id-ctxt e ctxt)))

(require (for-syntax racket/base syntax/parse))

;; quasiquote for art
(define-syntax (qq-art stx)
  (syntax-parse stx
    [(_ loc+id-ctxt expr)
     #:with expr* #'(set-id-ctxt (quasisyntax/loc loc+id-ctxt expr) (get-id-ctxt loc+id-ctxt))
     (quasisyntax/loc stx expr*)]))



(define (interpret5 exprs ctxt)
  ;; rsound play stream
  (define ps (make-pstream))
  (for ([expr exprs])
    (syntax-parse expr
      [({~datum tone} freq:number)
       (define/syntax-parse (_ the-start) (require-context (get-id-ctxt expr) #'start))
       (define/syntax-parse (_ the-end) (require-context (get-id-ctxt expr) #'end))

       (define/syntax-parse ({~datum volume} vol:number) (require-context ctxt #'volume))
       (unless (<= (syntax-e #'vol) 10)
         (raise-syntax-error 'interpret3 "too loud!!! volume must be lte 10" #'vol))
       (pstream-queue ps 
         (make-tone (syntax-e #'freq) (/ (syntax-e #'vol) 100) (seconds->frames (- (syntax-e #'the-end) (syntax-e #'the-start))))
         (+ (pstream-current-frame ps) (seconds->frames (syntax-e #'the-start))))])))


(define (note->tone3 n ctxt)
  (define rp 264)
  (syntax-parse n
    [({~datum note} pitch)
     (define/syntax-parse ({~datum tuning} tuning-name) (require-context ctxt #'tuning))
     (define tuner (match (syntax-e #'tuning-name) ['12tet equal-temper] ['just just-intone]))
     (qq-art
        n
        (tone #,(tuner (syntax-e #'pitch) rp)))]))

;; shorthand for horizontally composed
(define-syntax (-- stx)
  (syntax-parse stx
    [(_ s:number [t:number expr ...] ...)
     #`(append 
          #,@(for/fold ([acc '()] [t (syntax-e #'s)] #:result (reverse acc))
               ([es (syntax->list #'((expr ...) ...))] [dt (syntax->list #'(t ...))])
        (define t* (+ t (syntax-e dt)))
        (define/syntax-parse (e* ...) es)
        (println #'(e* ...))
        (values (cons #`(@2 (list #`(start #,#,t) #`(end #,#,t*)) #'e* ...) acc) t*)))]
    [(_ [t:number expr ...] ...) (syntax/loc stx (-- 0 [t expr ...] ...))]))

;; debugging (turn into sexpr with id ctxt)
(define (un-@ expr)
  #`(@ [#,@(get-id-ctxt expr)] #,expr))

(define (within? ctxt1 ctxt2)
  (define/syntax-parse (_ s1-) (require-context ctxt1 #'start))
  (define/syntax-parse (_ e1-) (require-context ctxt1 #'end))

  (define/syntax-parse (_ s2-) (require-context ctxt2 #'start))
  (define/syntax-parse (_ e2-) (require-context ctxt2 #'end))
  
  (match-define (list s1 e1 s2 e2) (map syntax-e (list #'s1- #'e1- #'s2- #'e2-)))
  
  (and (>= s1 s2) (<= e1 e2)))

(define (require-context/surrounding expr ctxt head)
  (define ctxt* (filter (λ (x) (within? (get-id-ctxt expr) (get-id-ctxt x))) ctxt))
  (define result (lookup ctxt* head))
  (or result
    (raise-syntax-error 'require-context (format "no ~s in context" (syntax-e head)) expr)))
    
(define (note->tone4 n ctxt)
  (define rp 264)
  (syntax-parse n
    [({~datum note} pitch)
     (define/syntax-parse ({~datum tuning} tuning-name) (require-context/surrounding n ctxt #'tuning))
     (define tuner (match (syntax-e #'tuning-name) ['12tet equal-temper] ['just just-intone]))
     (qq-art
        n
        (tone #,(tuner (syntax-e #'pitch) rp)))]))

(define (interpret6 exprs ctxt)
  ;; rsound play stream
  (define ps (make-pstream))
  (for ([expr exprs])
    (syntax-parse expr
      [({~datum tone} freq:number)
       (define/syntax-parse (_ the-start) (require-context (get-id-ctxt expr) #'start))
       (define/syntax-parse (_ the-end) (require-context (get-id-ctxt expr) #'end))

       (define/syntax-parse ({~datum volume} vol:number) (require-context/surrounding expr ctxt #'volume))
       (unless (<= (syntax-e #'vol) 10)
         (raise-syntax-error 'interpret3 "too loud!!! volume must be lte 10" #'vol))
       (pstream-queue ps 
         (make-tone (syntax-e #'freq) (/ (syntax-e #'vol) 100) (seconds->frames (- (syntax-e #'the-end) (syntax-e #'the-start))))
         (+ (pstream-current-frame ps) (seconds->frames (syntax-e #'the-start))))])))