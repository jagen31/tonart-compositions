#lang racket

(require video video/render)

(define the-clip (clip "feb2.mov"))

(render the-clip)