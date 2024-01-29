#lang racket
(require tonart)

(define-art scarlatti-1 
  (load-musicxml "compositions/scarlatti/aminor.musicxml" [right right-2 left])
  (musicxml->tonart))

(define-art scarlatti-2
  (load-musicxml "compositions/scarlatti/fsminor.musicxml" [right right-2 left])
  (musicxml->tonart))