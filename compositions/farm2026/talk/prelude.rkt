#lang racket
(require tonart)
(provide theme)

(define-art theme
  (load-musicxml "scores/prelude.musicxml" [one two three four])
  (musicxml->tonart))
