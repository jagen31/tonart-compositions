#lang racket

(require tonart)

(define-art the-rhythm (rhythm 1 1 0.5 0.5 1 1 2))
(define-art the-^s (seq (^s 1 2 3 4 5 4 3)))

(define-art the-rhythm2 (rhythm 1 1 0.5 0.5 1 1 2))
(define-art the-^s2 (seq (^s 1 2 3 4 5 4 3)))

(define-art the-opening
  (-- [8 the-rhythm the-^s (apply-rhythm)] [8 the-rhythm2 the-^s2 (apply-rhythm) (transpose-diatonic 1) (run-transpose-diatonic) (i@ [4 5] (dilate 2))]))

(define-art the-piece
  (voice@ (one) the-opening (octave 5))
  #;(voice@ (two) 
    (seq (chords [f 0 m] [c 0 M] [f 0 M])) (rhythm 8 8 8) (apply-rhythm)
    (-- [4 (hole)] [2] [1 (hole)] [1] [4 (hole)] [2] [1 (hole)] [1] [8 (hole)]) (fill-holes chord)
    (chord->notes/simple 3)))

(define-art-rewriter chuck-postlude
  (lambda (stx)
    #'(context 
        (sound-map) 

        (^->note) (note->midi)  (volume 5) 
        (voice@ (one) (channel 1)) (voice@ (two) (channel 2)) 
        (voice@ (three) (channel 3)) 
        (voice@ (four) (channel 1)) (voice@ (five) (channel 1))
        (midi->full-midi)
        (tempo 120) (apply-tempo)
        (d/dt))))

(define-art harmony 
  (seq (chords [a 0 m] [c 0 M] [d 1 M] [e 0 M])))

(define-art the-loop
  (loop 4 (-- [3 (^ 1)] [1 (^ 5)])))