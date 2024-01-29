#lang racket

(require tonart)

(define-art test (chord c 0 [M]))

(realize (quote-realizer) (i@ [0 4] test (instrument trumpet))
  (chord->notes/simple 3) (note->midi)   (instrument-map [trumpet . blah]))