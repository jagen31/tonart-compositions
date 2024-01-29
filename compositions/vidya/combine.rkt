#lang racket

(require video video/render)

(define-art clips
  (ix--
    (quart (clip "dec10b.mov"))
    (quart (clip "dec10a.mov"))))

(define-art expr
  (quart
    (playlist
      (unquart* clips (run-apl (apl:reverse *ctxt*)))))
  (run-unquart))