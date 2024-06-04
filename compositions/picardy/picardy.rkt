#lang racket

(require tonart)

(define-simple-rewriter picardy-tune x-picardy-tune
  picardy-rhythm picardy->^s (apply-rhythm))

(provide x-picardy-tune)

(define-art the-canon
  (voice@ (one) (picardy-tune) (octave 5)) 
  (voice@ (two) (picardy-tune) (translate 2) (octave 4)))

(define-art bassline
  (seq (^s 1 4 1 4 0 3 1 4 1 6 7 8 1 4 6 8 1 3 4 1))
  (rhythm 6 2 8 2 2 4 6 2 8 2 2 4 4 4 4 4 4 2 2 8)
  (apply-rhythm))

(define-art countermelody
  (seq (^s 1 0 -1 1 [0 1] 1 2 3 2 1 4 3 2 1 5 4 3 2 1 2 0 0 2 2))
  (i@ [0 24] (uniform-rhythm 1))
  (apply-rhythm))

(define-art fallen
  (seq (^s 2 2 [3 1] [3 1] 4 1 -1 -1 -2 [-1 1] [0 1] 2 2 [3 1] [3 1] 4 1 -3 0 -2))
  (rhythm 1 1 1 1 2 2 2 2 2 1 1 1 1 1 1 2 4 1 1 4)
  (apply-rhythm))

(define-art fallen2
  (seq (^s 8 8 7 7 [6 1] 6 5 4 3 4 5 8 8 8 8 7 6 2 [3 1]))
  (rhythm 1 1 1 1 2 2 2 2 2 1 1 1 1 1 1 2 4 2 4)
  (apply-rhythm))

(define-art the-piece-
  ;; pedal tone
  (measure@ [1 20] the-canon (voice@ (three) (^ 1) (octave 3)))
  (measure@ [21 40] the-canon
    (voice@ (three) bassline (octave 3))
    (voice@ (four)
      ;; FIXME jagen not aesthetic at all.  this just means- countermelody twice :(
      (i@ [0 48] (loop 24 countermelody) (expand-loop) (octave 5)))

    (measure@ [13 20] (voice@ (four) fallen (octave 5)) (voice@ (five) fallen2 (octave 4)))))

;; finishing touches
(define-art the-piece
  the-piece-
  ;; smooth transitions
  (voice@ (four)
    ;; add a sharp 3 to throw off the ear a bit, foreshadow later harmony
    (measure@ 27 (i@ [0 1] (delete ^) (^ 3 1)))
    ;; just sounds better
    (measure@ 32 (i@ [0 2] (delete ^) (-- [1 (^ 1)] [1 (^ 1)])))))

(realize (namespace-provide-realizer) (reify-art-definitions))
