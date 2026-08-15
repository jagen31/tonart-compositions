#lang racket

;; Bridge from the dance arm-diagram art objects to the lilypond `image`
;; art object: walks the context, renders each arm-diagram to a PNG via
;; 2htdp/image, drops the file into a resources folder, and replaces the
;; arm-diagram with `(image "…")` so the lilypond realizer hangs it above
;; the staff.

(require tonart "lilypond.rkt"
         "../dance/dance-art.rkt"
         (for-syntax syntax/parse racket/format racket/file racket/path racket/class
                     racket/draw
                     "../dance/dance-annotation.rkt"
                     (prefix-in im: 2htdp/image)
                     (only-in 2htdp/private/image-more render-image)))

(provide (all-defined-out)
         (all-from-out "../dance/dance-art.rkt"))

;; lilypond's default PostScript backend ignores PNG alpha and fills it
;; with white.  EPS, however, *does* honour `background-color = #f` in the
;; PostScript backend (per the lilypond docs), so we render the dancers
;; to EPS via racket/draw's post-script-dc% instead.
(define-for-syntax (save-image-eps! img path)
  (define w (inexact->exact (ceiling (im:image-width img))))
  (define h (inexact->exact (ceiling (im:image-height img))))
  (define dc (new post-script-dc%
                  [interactive #f]
                  [width w] [height h]
                  [output path]
                  [as-eps #t]))
  (send dc start-doc "tonart-arm-diagram")
  (send dc start-page)
  (send dc set-smoothing 'aligned)
  (render-image img dc 0 0)
  (send dc end-page)
  (send dc end-doc))

(define-for-syntax (render-arm-png! l r facing-sym disk-path)
  (unless (file-exists? disk-path)
    (define img (make-dancer l r facing-sym))
    (save-image-eps! img (path->string disk-path))))

;; (arm-diagrams->images "<disk-dir>")
;;   write images into <disk-dir> (e.g. "compositions/lilypond/resources")
;;   and reference them in the resulting (image …) using the same
;;   directory.
;;
;; (arm-diagrams->images "<disk-dir>" "<ref-prefix>")
;;   write to <disk-dir> on disk but reference as "<ref-prefix>/<file>" in
;;   the .ly output verbatim.  use this only when you specifically need
;;   relative refs (e.g. a tarball distribution); otherwise prefer the
;;   one-arg form below.
;;
;; In the one-arg form (and when ref-prefix is omitted), the .ly's
;; \epsfile path is the *absolute* path of disk-dir resolved against
;; racket's cwd at rewrite time.  Absolute refs are valid regardless of
;; where lilypond is later invoked from — important for the scribble
;; flow, which cd's into scribble-resources/ to compile the .ly.
(define-art-rewriter arm-diagrams->images
  (syntax-parser
    [(_ disk-dir:str (~optional ref-prefix:str))
     (define dir (syntax-e #'disk-dir))
     (define ref
       (if (attribute ref-prefix)
           (syntax-e #'ref-prefix)
           (path->string (path->complete-path dir))))
     (make-directory* dir)
     (define (emit-image! l-num r-num f-sym expr adds dels)
       (define filename (format "arm-~a-~a-~a.eps" l-num r-num f-sym))
       (render-arm-png! l-num r-num f-sym (build-path dir filename))
       (define ref-path (format "~a/~a" ref filename))
       (values (cons (qq-art expr (image #,ref-path)) adds)
               (cons (delete-expr expr) dels)))
     (define-values (additions deletions)
       (for/fold ([adds '()] [dels '()]
                  #:result (values (reverse adds) (reverse dels)))
                 ([expr (current-ctxt)])
         (syntax-parse expr
           ;; full-arm-diagram already carries facing — no context lookup
           [({~literal full-arm-diagram} l:number r:number f:id)
            (emit-image! (syntax-e #'l) (syntax-e #'r) (syntax-e #'f)
                         expr adds dels)]
           [({~literal arm-diagram} l:number r:number)
            (define facing-expr (require-context (lookup-ctxt) expr #'facing))
            (syntax-parse facing-expr
              [({~literal facing} f:id)
               (emit-image! (syntax-e #'l) (syntax-e #'r) (syntax-e #'f)
                            expr adds dels)]
              [_ (values adds dels)])]
           [_ (values adds dels)])))
     #`(context #,@deletions #,@additions)]))
