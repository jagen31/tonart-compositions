#lang racket

;; A tiny play-script art for tonart.
;;
;; Wrap a sequence of `(line CHARACTER "...")` and
;; `(stage-direction "...")` art objects in a `(script ...)`
;; embedding so the scribble realizer can pull them out as a single
;; block of dialogue.
;;
;; Example:
;;
;;   (script
;;     (line alice "Hello, Bob.")
;;     (stage-direction "Bob waves.")
;;     (line bob   "Hi, Alice."))
;;
;; Each `line`'s character is an identifier (rendered upper-cased and
;; bolded as the speaker name); the text is a string.  Each
;; `stage-direction` is a string that scribble renders italicised, in
;; the convention of stage directions in printed plays.  We avoid the
;; name `direction` to keep clear of tonart's musicxml `direction`
;; art-object.

(require tonart (for-syntax syntax/parse))

(provide (all-defined-out))

;; one spoken line: CHARACTER followed by a single string of dialogue.
(define-art-object (line [character text]))

;; a stage direction (italics in the rendered output).
(define-art-object (stage-direction [text]))

;; container that gathers the lines/directions of a single scene.
;; behaves exactly like tonart's `music` embedding: passes the body
;; through unchanged, but keeps the items grouped under one (script
;; ...) syntax node so the scribble realizer can format them as a
;; cohesive dialogue block.
(define-art-embedding (script [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (@ () expr ...)))])))
