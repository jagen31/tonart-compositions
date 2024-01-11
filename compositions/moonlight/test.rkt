#lang racket

(require art art/timeline art/sequence/ravel tonart rsound)

(set-output-device! 1)

(define-art mniip
  (i@ [0 32] 
    (tones 440 1320) (uniform-rhythm 8) (rhythm->holes) (fill-holes tone)
    (seq (tones 881.8 881.2 880.6 880)) (uniform-rhythm 8) (apply-rhythm)))
