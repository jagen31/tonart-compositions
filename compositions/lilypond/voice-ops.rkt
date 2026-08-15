#lang racket

;; Voice-level rewriters for the lilypond pipeline.
;;
;; (merge-voices from into)
;;   Rewrite every expression in voice `from` so it lives in voice
;;   `into` instead.  After this rewriter runs, no expression carries
;;   the `from` voice tag any more.  Useful for collapsing musicxml
;;   stems like `right` + `right-2` into a single `right` staff.

(require tonart "lilypond.rkt"
         (for-syntax syntax/parse syntax/id-set))
(provide (all-defined-out))

(define-art-rewriter merge-voices
  (λ (stx)
    (syntax-parse stx
      [(_ from:id into:id)
       (define from-id #'from)
       (define into-id #'into)
       (define-values (dels adds)
         (for/fold ([dels '()] [adds '()]
                    #:result (values (reverse dels) (reverse adds)))
                   ([expr (current-ctxt)])
           (define voices (expr-voice expr))
           (cond
             [(memf (λ (v) (free-identifier=? v from-id)) voices)
              ;; musicxml fills the silent stem with whole-bar rests
              ;; that, once merged, would overlap and confuse the
              ;; realizer.  Drop music-rests from the source voice and
              ;; let the realizer's gap-fill regenerate any actually
              ;; needed rests.
              (cond
                [(syntax-parse expr
                   [({~datum music-rest} . _) #t]
                   [_ #f])
                 (values (cons (delete-expr expr) dels) adds)]
                [else
                 ;; rewrite the (voice ...) coordinate in this expr's
                 ;; id-ctxt: swap from-id for into-id and dedup.
                 (define new-id-ctxt
                   (for/list ([coord (in-list (get-id-ctxt expr))])
                     (syntax-parse coord
                       [({~datum voice} v ...)
                        (define remapped
                          (for/list ([id (in-list (syntax->list #'(v ...)))])
                            (if (free-identifier=? id from-id) into-id id)))
                        (define seen (mutable-free-id-set))
                        (define dedup
                          (for/list ([id (in-list remapped)]
                                     #:unless (free-id-set-member? seen id))
                            (free-id-set-add! seen id)
                            id))
                        (qq-art coord (voice #,@dedup))]
                       [_ coord])))
                 (values (cons (delete-expr expr) dels)
                         (cons (set-id-ctxt expr new-id-ctxt) adds))])]
             [else (values dels adds)])))
       #`(context #,@dels #,@adds)])))
