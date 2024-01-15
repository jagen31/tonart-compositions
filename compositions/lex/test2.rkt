#lang racket

(require art tonart tonart/rsound "test.rkt")

(define-art result 
  (m! song) (key c 0 minor) (^o->note) 
  (tuning 12tet) (note->tone) (volume 5) (tempo 120) (apply-tempo))
