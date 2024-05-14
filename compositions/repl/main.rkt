#lang racket

(require (except-in art string) (except-in tonart string) tonart/common-practice (only-in pict pict->bitmap)
  2htdp/image
  #;tonart/private/electronic/chuck/lib
  (except-in racket/gui make-color make-pen send)
  (prefix-in gui: (only-in racket/gui send))
  (for-syntax data/gvector racket/match racket/list syntax/parse racket/string racket/format))

(begin-for-syntax (struct art-zipper [edit-ctxt eval-ctxt ctxt-type focus] #:transparent))

(define-for-syntax the-context (box (art-zipper '() '() '() (gvector))))

(define the-image (box empty-image))
(define the-context-source (box ""))
(define the-eval-ctxt-image (box empty-image))
(define the-source (box ""))

(define-for-syntax (art-zipper-enter ctxt ix)
  (match ctxt
    [(art-zipper edit-ctxt eval-ctxt ctxt-type focus)
     (define focus* (gvector->list focus))
     (define-values (l r-) (split-at focus* ix))
     (define-values (selected r) (split-at r- 1))
     ;; evaluate if we're within a subcontext
     (define eval-ctxt* 
       (if (or (null? ctxt-type) (car ctxt-type))
         (run-art-exprs l '() (apply append (reverse eval-ctxt)))
         '()))
     #;(define items (run-art-expr (gvector-ref focus ix) '() eval-ctxt* #:partial #t))
     (define ctxt* 
       (syntax-parse (gvector-ref focus ix)
         [(name:id expr ...)
          #:when (embed/s? (syntax-local-value #'name (λ() #f)))
          (art-zipper
            (cons (cons l r) edit-ctxt) (cons eval-ctxt* eval-ctxt) (cons #'name ctxt-type)
            (list->gvector (syntax->list #'(expr ...))))]
         [(expr ...)
          (art-zipper
            (cons (cons l r) edit-ctxt) (cons eval-ctxt* eval-ctxt) (cons #f ctxt-type)
            (list->gvector (syntax->list #'(expr ...))))]))
     ctxt*]))

(define-for-syntax (art-zipper-exit ctxt)
  (match ctxt
    [(art-zipper edit-ctxt eval-ctxt ctxt-type focus)
     (define focus* (gvector->list focus))
     (define-values (frame typ) (values (car edit-ctxt) (car ctxt-type)))
     ;; FIXME jagen source loc
     (define ctxt* 
       (art-zipper (cdr edit-ctxt) (cdr eval-ctxt) (cdr ctxt-type) 
         (list->gvector 
           (append 
             (car frame) 
             (list (if typ #`(#,typ #,@focus*) #`(#,@focus*)))
             (cdr frame)))))
     ctxt*]))

(define edit-ctxt-frame (new frame% [label "Edit Context"] [width 800] [height 400]))
(new canvas% 
  [parent edit-ctxt-frame]
  [paint-callback
   (λ (canvas dc)
     (gui:send dc draw-bitmap (pict->bitmap (text (unbox the-context-source) 24 'black)) 0 0))])
(gui:send edit-ctxt-frame show #t)

(define eval-ctxt-frame (new frame% [label "Eval Context"] [width 800] [height 400]))
(new canvas% 
  [parent eval-ctxt-frame]
  [paint-callback
   (λ (canvas dc)
     (gui:send dc draw-bitmap (pict->bitmap (unbox the-eval-ctxt-image)) 0 0))])
(gui:send eval-ctxt-frame show #t)

(define input-frame (new frame% [label "Focus"] [width 800] [height 400]))
(new canvas% 
  [parent input-frame]
  [paint-callback
   (λ (canvas dc)
     (gui:send dc draw-bitmap (pict->bitmap (text (unbox the-source) 24 "black")) 0 0))])
 
(gui:send input-frame show #t)

(define output-frame (new frame% [label "Output (draw-realizer)"] [width 800] [height 400]))
(new canvas% 
  [parent output-frame]
  [paint-callback
   (λ (canvas dc)
     (gui:send dc draw-bitmap (pict->bitmap (unbox the-image)) 0 0))])
(gui:send output-frame show #t)

(define-for-syntax (exprs->string exprs #:numbers [numbers #t])
  (string-join 
    (for/list ([expr exprs] [i (in-naturals)])
      (if numbers (format "~a: ~a" i (syntax->datum expr)) (~s (syntax->datum expr)))) "\n"))

;; FIXME jagen bad boy diamond recursion
(define-for-syntax (edit-context->string edit-ctxt)

  (define (edit-context->string frames types)
    (cond 
      [(null? frames)
      ;; FIXME jagen WRONG
      "\n▢\n"]
      [else
       (define core
         (string-append
           (exprs->string (caar frames) #:numbers #f) 
           (edit-context->string (cdr frames) (cdr types))
           (exprs->string (cdar frames) #:numbers #f)))
       (if (car types) 
         (string-append (format "(~s" (syntax-e (car types))) "\n" core ")")
         core)]))

  (string-append
    (edit-context->string 
      (reverse (art-zipper-edit-ctxt edit-ctxt)) (reverse (art-zipper-ctxt-type edit-ctxt)))
    "\n"))

(define-syntax (compile _) 

  (match-define (art-zipper _ _ the-types the-focus) (unbox the-context))
  (define the-type (for/or ([t the-types]) t))

  #`(begin
      (set-box! the-image 
        #,(cond 
          [(null? the-types) 
           #`(realize (draw-realizer [800 200]) #,@(gvector->list the-focus))]
          [(car the-types)
           #`(realize (draw-realizer [800 200]) 
             (#,(car the-types) #,@(gvector->list the-focus)))]
          [else #'empty-image]))

      (define the-text #,(exprs->string (gvector->list the-focus)))
      (set-box! the-source the-text)

      ;; FIXME jagen multiple nesting levels
      (define the-context-text #,(edit-context->string (unbox the-context)))
      (set-box! the-context-source the-context-text)

      (set-box! the-eval-ctxt-image
        #,(if (not the-type) 
            #'empty-image
            #`(realize (draw-realizer [800 200]) 
                (#,the-type
                 #,@(apply append (reverse (art-zipper-eval-ctxt (unbox the-context))))))))

      (gui:send output-frame refresh)
      (gui:send input-frame refresh)
      (gui:send edit-ctxt-frame refresh)
      (gui:send eval-ctxt-frame refresh)))
   
(compile)

(define-art-object (comment []))

(define-syntax (#%top-interaction stx)
  (define the-exprs (art-zipper-focus (unbox the-context)))
  (syntax-parse stx
    [(_ {~datum enter} n:number)
     (set-box! the-context (art-zipper-enter (unbox the-context) (syntax-e #'n)))
     #'(compile)]
     [(_ {~datum exit})
     (set-box! the-context (art-zipper-exit (unbox the-context)))
     #'(compile)]
    [(_ {~datum add} expr)
     (gvector-add! the-exprs #'expr)
     #'(compile)]
    [(_ {~datum add} n:number expr)
     (gvector-insert! the-exprs (syntax-e #'n) #'expr)
     #'(compile)]
    [(_ {~datum set} n:number expr)
     (gvector-set! the-exprs (syntax-e #'n) #'expr)
     #'(compile)]
    [(_ {~datum remove} i:number)
     (gvector-remove! the-exprs (syntax-e #'i))
     #'(compile)]
    [(_ {~datum comment} n:number)
     #:with expr (gvector-ref the-exprs (syntax-e #'n))
     (gvector-set! the-exprs (syntax-e #'n) #'(comment expr))
     #'(compile)]
     [(_ {~datum show})
      (displayln (exprs->string (gvector->list the-exprs)))
      #'(void)]
    [(_ {~datum uncomment} n:number)
     (define expr 
       (syntax-parse (gvector-ref the-exprs (syntax-e #'n))
         [({~literal comment} expr) #'expr]))
     (gvector-set! the-exprs (syntax-e #'n) expr)
     #'(compile)]
    [(_ {~datum !} expr ...)
     #:with *focus* (datum->syntax stx '*focus*)
     #:with *context* (datum->syntax stx '*context*)
     #`(begin 
         (define-art *focus* #,@(gvector->list the-exprs)) 
         (define-art *context* #,@(apply append (reverse (art-zipper-eval-ctxt (unbox the-context)))))
         expr ...)]))
