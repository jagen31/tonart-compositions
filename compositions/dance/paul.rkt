#lang racket

(require tonart "dance-art.rkt")

(define-art ddd-dance
  (seq (facing towards) (arm-diagrams [12 6] [6 6] [3 6])))

(define-art ddd-rhythm (rhythm 2 4 1))

(realize (staff-realizer [800 200] {})
  (voice@ [soprano]
          ddd-dance
          (rewrite-in-seq (arm-diagram->full-arm-diagram) (delete facing))
          ddd-rhythm
          (apply-rhythm)
          ))
  