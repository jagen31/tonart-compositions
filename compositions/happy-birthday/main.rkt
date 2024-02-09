#lang racket

(require tonart "hbd.rkt")

(define-art-rewriter postlude
  (λ (stx)
    #'(context 
        (key c 0 major) (^->note) (tempo 120) (apply-tempo) 
        (instrument |Clarinet|) (note->midi))))

(rs-write (mrsr (seq (ix-- (music bass-var) (music block-chords-var) (music broken-chords-var) (music waltz-chords-var))) (inline-music-seq) (postlude)) "out.wav")
