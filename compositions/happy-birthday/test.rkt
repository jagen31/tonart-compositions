#lang racket

(require tonart "hbd.rkt")

(define-art-rewriter postlude
  (λ (stx)
    #'(context 
        (key c 0 major) (^->note) (tempo 120) (apply-tempo) 
        (run-transpose-diatonic)
        (tuning 12tet) (note->tone) (voice@ (melody) (volume 6)) (voice@ (accomp) (volume 4)))))

(define-art compiled 
  (seq (ix-- 
    (music bass-var) (music block-chords-var) (music broken-chords-var) 
    (music waltz-var) (music alberti-bass-var) (music march-var) (music ballad-var)))
  (inline-music-seq))

(rs-write (mrsr compiled (octave 4) (postlude)) "out.wav")
