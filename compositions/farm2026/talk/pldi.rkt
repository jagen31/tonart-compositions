#lang racket
(require tonart)
(provide theme)

(define-art theme
  ;; Load the single part into tonart's default voice `main`, not a
  ;; `melody` voice: `musicxml->tonart` stamps every note with this
  ;; name, and lyrics with no `voice@` default to `main`.  A mismatch
  ;; here orphans the lyrics -- the lilypond realizer groups lyrics by
  ;; voice and silently drops any whose voice carries no notes.
  (load-musicxml "scores/pldi.musicxml" [main])
  (musicxml->tonart))
