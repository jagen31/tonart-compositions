#lang racket

(require tonart art/coordinate/instant "dance-annotation.rkt" (prefix-in im: 2htdp/image)
         art/coordinate/name
         (for-syntax syntax/parse racket/match
                     (only-in art/private/core get-id-ctxt context-ref)
                     art/coordinate/name))

(define-art-object (arm-diagram []))
(define-art-object (facing []))
(define-art-object (full-arm-diagram []))

;; (pick-variant <name>) — delete everything in the current
;; context that carries a `name@`-imposed `(name <other>)` coord
;; on its id-ctxt, leaving the chosen variant (plus all unnamed
;; material) intact.  Use in a per-realizer hook, e.g. the
;; scribble/lilypond pipeline: `(rewrite-in-music (pick-variant a))`
;; before the realizer runs, so a score that ships TWO `name@`-tagged
;; dance choreographies (one for live coding to switch between) still
;; renders cleanly on the staff with just one choreography's arm
;; figures hanging above the notes instead of both stacking on top of
;; each other.  Untagged exprs (notes, lyrics, untagged hints, etc.)
;; are always preserved.
(define-art-rewriter pick-variant
  (λ (stx)
    (syntax-parse stx
      [(_ wanted:id)
       (define wanted-sym (syntax-e #'wanted))
       (define dels
         (for/list ([e (in-list (current-ctxt))]
                    #:when (let* ([ctx (get-id-ctxt e)]
                                  [n (and ctx (context-ref ctx #'name))])
                             (and n
                                  (syntax-parse n
                                    [({~literal name} sym . _)
                                     (not (eq? (syntax-e #'sym) wanted-sym))]
                                    [_ #f]))))
           (delete-expr e)))
       #`(context #,@dels)])))

(define-art-rewriter arm-diagrams
  (λ (stx)
    (syntax-parse stx
      [(_ [l:number r:number] ...)
       (qq-art stx (ix-- (arm-diagram l r) ...))])))

(define-art-rewriter facings
  (λ (stx)
    (syntax-parse stx
      [(_ f* ...)
       (define/syntax-parse (face ...)
         (for/list ([f (syntax->datum #'(f* ...))])
           (match f ['t 'towards] ['a 'away] ['l 'left] ['r 'right])))
       (qq-art stx (ix-- (facing face) ...))])))

(define-mapping-rewriter (arm-diagram->full-arm-diagram [(: d arm-diagram)])
  (λ (stx d)
    (syntax-parse d
      [(_ l r)
       (define/syntax-parse (_ orient) (require-context (lookup-ctxt) d #'facing))
       (qq-art d (full-arm-diagram l r orient))])))
                                                        


(define-drawer full-arm-diagram-drawer
  (λ (e)
    (syntax-parse e
      [({~literal full-arm-diagram} l r orient)
       #'(im:scale 1/3 (make-dancer l r 'orient))]
      [_ #f])))

(register-drawer! full-arm-diagram full-arm-diagram-drawer)

(provide (all-defined-out))
