#lang racket

(require tonart (for-syntax syntax/parse) "organ-config.rkt")
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-mapping-rewriter (fill-random-index [(: h hole)])
  (λ (stx h)
    (syntax-parse stx
      [(_ n:number)
       (define/syntax-parse val (random (syntax->datum #'n)))
       (qq-art h (! val))])))

(define-art cs (seq (chords [c 0 M] [a 0 M] [d 0 M] [b -1 M])))
(define-art r (rhythm 1 1/4 1/4 1/2 1/4 1/4 1/2 1))

(define-art r2 (i@ [0 4] (urhy 1/8)))


(define-art plane-beat 
 (name@ a (i@ [0 4] (urhy 1)) (sound snare) (rhythm->holes) (fill-holes sound)) 
 (translate 1/2) (name@ c (rhythm 1 1/4 3/4 1.25 0.75) (sound kick) (rhythm->holes) (fill-holes sound)) 
 (name@ b (i@ [0 4] (urhy 1/4)) (sound cymbal) (rhythm->holes) (fill-holes sound)))

(define-art-rewriter plane-noise
  (λ (stx)
    (syntax-parse stx
      [(_ r)
       (qq-art stx
         (context
  (voice@ [noise] (note-range [a 4] [a 5]) cs 
    (rhy* 8 8 8 8) (chord->scalar-note-seq) 
    (-- [32 (loop 4 r)]) (expand-loop) (rhythm->holes)
    (fill-random-index 5) (seq-ref))))])))

(define-art plane-accomp (plane-noise r2) (i@ [0 32] (loop 4 plane-beat)) (expand-loop))

(define-art waltz-bass (seq (^s 1 -2)) (rhy* 3 3) (dilate 1/2))
(define-art waltz-chord-rhy (-- [1] [1 (hole)] [1 (hole)] [1] [1 (hole)] [1 (hole)]) (dilate 1/2))

(define-art waltz-ch (-- [6 (chord c 0 [m])] [6 (chord g 0 [M])] [6 (chord c 0 [m])] 
    [3 (chord f 0 [m])] [3 (chord g 0 [M])] [6 (chord c 0 [m])]))
(define-art waltz-key (-- [6 (key c 0 minor)] [6 (key g 0 major)] [6 (key c 0 minor)] 
    [3 (key f 0 minor)] [3 (key g 0 major)] [6 (key c 0 minor)]))

(define-art-rewriter waltz-n
  (λ (stx)
    (syntax-parse stx
      [(_ n)
       (qq-art stx
         (context
          (voice@ [bass] (manual P) (i@ [0 n] (loop 3 waltz-bass)) (-- [30 waltz-key] [30 waltz-key]) (expand-loop) (octave 3) (^->note)) 
          (voice@ [accomp] (manual II) (i@ [0 n] (loop 3 waltz-chord-rhy)) (-- [30 waltz-ch] [30 waltz-ch]) (expand-loop) (fill-holes chord) (chord->notes/simple 4))))])))

(define-art waltz-cresc
  (-- [12 reg] [18 reg2] [18 reg3] [12 reg4]))

(define-art waltz-decresc
  (-- [12 reg4] [18 reg3] [18 reg2] [44 reg]))