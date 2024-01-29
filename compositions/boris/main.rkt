#lang racket

(require art tonart "song.rkt")

(define-art sample-song-for-perf
  song-in-g-minor
  (voice@ (melody-v) 
    (rhythm 16 16) (apply-rhythm) 
    (i@ [0 30] (loop 6 (-- [0.5 (! 0)] [1 (! 1)] [0.5 (! 2)] [2 (! 0)] [1 (! 1)] [1 (! 2)])) (expand-loop))
    (seq-ref) (octave 5) (^->note))
  (voice@ (accomp-v) 
    (rewrite-in-seq (rewrite-in-music (i@ [0 16] (repeat 4) (expand-repeat)) (chord->notes/simple 3)))
    (inline-music-seq))
  (i@ [0 64] (repeat 32) (expand-repeat)))

  (define sound 
    (mrsr sample-song-for-perf
      (voice@ (accomp-v) (inst |Yamaha Grand Piano|) (note->midi)) 
      (voice@ (melody-v) (note->midi) (inst Flute)) (apply-tempo)))