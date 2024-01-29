#lang racket

(require tonart (prefix-in ta: tonart) art/sequence/ravel art/coordinate/instant
         (for-syntax syntax/parse racket/set racket/match) (prefix-in im: 2htdp/image) art/sequence/ravel open-app pict/code (only-in pict pict->bitmap))



(define-art-object (rkt [code]))

(define-drawer racket-drawer
  (λ (e)
    (syntax-parse e
      [({~literal rkt} expr ...)
       #`(begin (get-current-code-font-size (λ () 32))
                (im:overlay (pict->bitmap (code expr ...))
                            (im:rectangle #,(drawer-width) #,(drawer-height) 'solid 'transparent)))]
      [_ #f])))

(register-drawer! rkt racket-drawer)

(define-art-realizer assemble-racket-mod
  (λ (stx)
    #;(define sorted-ctxt (sort (current-ctxt) < #:key (λ (x) (expr-single-index x))))
    (define result 
      (for/fold ([acc '()] #:result (reverse acc))
                ([expr (current-ctxt)])
        (syntax-parse expr
          [({~literal rkt} code ...) (append (reverse (syntax->list #'(code ...))) acc)]
          [_ acc])))
    #`(module example racket #,@result (provide (all-defined-out)))))

(define-art-realizer assemble-racket-tl
  (λ (stx)
    #;(define sorted-ctxt (sort (current-ctxt) < #:key (λ (x) (expr-single-index x))))
    (define result 
      (for/fold ([acc '()] #:result (reverse acc))
                ([expr (current-ctxt)])
        (syntax-parse expr
          [({~literal rkt} code ...) (append (reverse (syntax->list #'(code ...))) acc)]
          [_ acc])))
    #`(begin #,@result)))

(define-art-realizer assemble-racket-timed
  (λ (stx)
    (define result 
      (for/fold ([acc '()] #:result (reverse acc))
                ([expr (current-ctxt)])
        (syntax-parse expr
          [({~literal rkt} code ...)
           (println (un-@ expr))
           (define t (syntax-parse (context-ref (get-id-ctxt expr) #'instant) [(_ t) (syntax-e #'t)] [_ #f]))
           (if t
               (cons #`(thread (λ () (begin (sleep #,t) #,@(syntax->list #'(code ...))))) acc)
               acc)]
          [_ acc])))
    (println "HERE!")
    (println #`(begin #,@result))
    #`(begin #,@result)))

(define-syntax (!rkt stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(realize (assemble-racket-tl) expr ...)]))

(define-syntax (!rktt stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(realize (assemble-racket-timed) expr ...)]))

(define-syntax (->mod stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(realize (assemble-racket-mod) expr ...)]))

(define-syntax (show stx)
  (syntax-parse stx
    [(_ [w h] expr ...)
     #`(begin (im:save-image (dr [w h] expr ...) "foo.png") (open-app "foo.png"))]))



(define-art-object (file [name]))

(define file-image (im:bitmap "file.png"))

(define-drawer file-drawer
  (λ (e)
    (syntax-parse e
      [({~literal file} name)
       #`(begin (im:overlay
                 (im:text (~a name) 24 'blue)
                 file-image
                 (im:rectangle #,(drawer-width) #,(drawer-height) 'solid 'transparent)))]
      [_ #f])))

(register-drawer! file file-drawer)


(define-art-realizer ls-realizer
  (λ (stx)
    #`(list #,@(for/list ([f (context-ref* (current-ctxt) #'file)])
                 (syntax-parse f
                   [(_ name) (syntax-e #'name)])))))

(define-syntax (ls stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(realize (ls-realizer) expr ...)]))


(define-mapping-rewriter (file->open-code [(: f file)])
  (λ (stx f)
    (syntax-parse f
      [(_ name) (qq-art f (rkt (begin (require open-app) (open-app name))))])))

(define-art-object (new-slide []))

(require (prefix-in ss: slideshow))

(define-mapping-rewriter (new-slide->slideshow-code [(: ns new-slide)])
  (λ (stx ns)
    (syntax-parse ns
      [(new-slide expr ...) (qq-art ns (rkt (begin (ss:slide expr ...) (ss:start-at-recent-slide))))])))

(define-mapping-rewriter (transpose-octave [(: n note)])
  (λ (stx n)
    (syntax-parse stx
      [(_ n*:number)
       (syntax-parse n
         [(_ p a o) (qq-art n (note p a #,(+ (syntax-e #'n*) (syntax-e #'o))))])])))

(define-art-rewriter function->contour
  (λ (stx)
    (syntax-parse stx
      [(_ w h)
       (qq-art stx (function->image w h))])))
  
(define-art-rewriter contour->notes 
  (λ (stx)
    (qq-art stx 
      (context
        (image->point-set)
        (chord->scalar-note-seq)
        (rhythm->holes)
        (fill-holes-from-points)
        (seq-ref)
        (delete point-set)))))

;; a staff-realizer which is set up for our demo voices
(define-art-realizer staff-realizer
  (λ (stx)
    (syntax-parse stx
      [(_ [w h])
       #:with soprano (datum->syntax stx 'soprano)
       #:with countermelody (datum->syntax stx 'countermelody)
       #:with accomp (datum->syntax stx 'accomp)
       #:do [(define num-voices (length (set->list (apply set (map (λ (x) (syntax->datum (car (expr-voice x)))) (filter (compose not null? expr-voice) (current-ctxt)))))))]
       #`(realize (ta:staff-realizer [w h] 
                    {[soprano treble] [descant treble] [accomp treble]
                     [bass bass]
                     [one bass] [two treble] [three bass]})
           #,@(current-ctxt))])))

;; shorthand for staff realizer
(define-syntax !score
  (syntax-parser
    [(_ [w:number h:number] expr ...)
     #'(realize (staff-realizer [w h]) expr ...)]
    [(_ expr ...)
     #'(!score [1800 600] expr ...)]))


(provide (all-defined-out))