#lang racket

(require tonart (prefix-in plot: plot) (prefix-in im: 2htdp/image) racket/draw
         (for-syntax data/gvector racket/class racket/draw racket/math syntax/parse (prefix-in plot: plot/no-gui) images/compile-time))

(define-art-object (function [vars form]))
(define-art-object (bitmap [data]))
(define-art-object (point-set [data]))

(define-drawer bitmap-drawer
  (λ (e)
    (syntax-parse e
      [(_ bs)
       #'(im:rotate 0 (read-bitmap (open-input-bytes bs) 'png/alpha))])))

(register-drawer! bitmap bitmap-drawer)

(define-for-syntax (save-png bm)
  (define p (open-output-bytes))
  (send bm save-file p 'png #:unscaled? #t)
  (define bs (get-output-bytes p))
  bs)

(define-mapping-rewriter (function->image [(: fn function)])
  (λ (stx fn)
    (syntax-parse stx
      [(_ bound ...)
       (syntax-parse fn
         [({~literal function} (var ...) form)
          (define f (eval-syntax #'(plot:function (λ (var ...) form) bound ...)))
          (qq-art fn
            (bitmap #,(parameterize ([plot:plot-width    150]
                                     [plot:plot-height   150]
                                     [plot:plot-x-label  #f]
                                     [plot:plot-y-label  #f]
                                     [plot:plot-x-ticks plot:no-ticks]
                                     [plot:plot-y-ticks plot:no-ticks]
                                     [plot:plot-line-width 0]
                                     [plot:plot-foreground-alpha 0]
                                     [plot:plot-background-alpha 0])
                        (save-png (plot:plot-bitmap f)))))])])))

(define-mapping-rewriter (image->point-set [(: im bitmap)])
  (λ (stx bm)
    (syntax-parse bm
      [(_ bs)
       (define image (read-bitmap (open-input-bytes (syntax-e #'bs)) 'png/alpha))
       (define width (send image get-width))
       (define height (send image get-height))
       (define pos (gvector))
       (for* ([x (in-range width)] [y (in-range height)])
         (define it (make-bytes 4))
         (send image get-argb-pixels x y 1 1 it)
         (when (ormap (λ (x) (not (= x 255))) (list (bytes-ref it 1) (bytes-ref it 2) (bytes-ref it 3)))
           (gvector-add! pos #`[#,x #,y])))
       (println (length (gvector->list pos)))
       (qq-art bm (point-set #,width #,height #,@(gvector->list pos)))])))

(define-mapping-rewriter (point-set->image [(: ps point-set)])
  (λ (stx fn)
    (syntax-parse stx
      [(_ bound ...)
       (syntax-parse fn
         [({~literal point-set} w h [x y] ...)
          (define f (eval-syntax #'(plot:points (map vector (list x ...) (list y ...)))))
          (qq-art fn
            (bitmap #,(parameterize ([plot:plot-width    150]
                                     [plot:plot-height   150]
                                     [plot:plot-x-label  #f]
                                     [plot:plot-y-label  #f]
                                     [plot:plot-x-ticks plot:no-ticks]
                                     [plot:plot-y-ticks plot:no-ticks]
                                     [plot:plot-line-width 0]
                                     [plot:plot-foreground-alpha 0]
                                     [plot:plot-background-alpha 0])
                        (save-png (plot:plot-bitmap f)))))])])))

(define-art-realizer graph-realizer
  (λ (stx)
    (syntax-parse stx
      [(_ bound ...)
       #:with (renderer ...)
         (for/fold ([acc '()])
                   ([expr (current-ctxt)])
           (syntax-parse expr
             [({~literal function} (var ...) form)
              (cons #'(plot:function (λ (var ...) form) bound ...) acc)]))
       #'(plot:plot (list renderer ...))])))

(dmr
 (i@ [0 4]
  (voice@ (soprano)
    (function (x) (+ (sin x) (sin (* 2 x)))))
  (voice@ (alto)
    (function (x) (- (sin x))))
  (voice@ (tenor)
    (function (x) (+ (* 2 x) 2)))
  (function->image pi (- pi))
  (image->point-set)
  (point-set->image)))