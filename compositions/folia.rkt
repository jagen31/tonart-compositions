#lang racket

(require tonart tonart/private/common-practice/transform art/sequence/ravel 2htdp/image)

(define-art folia
  (transforms [N] [N] [S P S] [N P] [P N] [S P S] [N] [N]))
