#lang racket

(require art tonart)

(define-art harmony (relative-chords [major] [M 7] [minor] [m 2] [major] [M 7] [minor])) 
(define-art harmonic-rhythm (rhythm 0.25 1.75 0.25 1.75))
(define-art accomp-rhythm (rhythm 0.25 0.25 0.25 0.25 1 0.25 0.25 0.25 0.25 1))

(define-art accomp 
  (seq 
    ;; FIXME jagen review this "idiom" of giving something a name temporarily ^_^
    (ix-loop 2 (music harmony (name@ hr harmonic-rhythm) accomp-rhythm)) (expand-ix-loop)
    ;; FIXME jagen intervals? also use APL somehow?
    (ix-- (rewrite-in-music (^ 2 -1)) (rewrite-in-music (^ 6)))))

;; FIXME jagen constraint that these happen in descending order in the same octave
(define-art melody (seq (ix-- (seq (^s 9 8 5)) (seq (^s 8 5 3)))))

;; this is the composition
(define-art song (voice@ (melody-v) melody) (voice@ (accomp-v) accomp) (tempo 100))

;; this is for convenience.  Transpose to change key
(define-art song-in-g-minor
  song (key g 0 minor)
  (voice@ (melody) (rewrite-in-seq (rewrite-in-seq (^->pitch))))
  (voice@ (accomp-v) 
    (rewrite-in-seq 
      (rewrite-in-music
        (^->pitch) (relative-chords->seq) 
        (name@ hr (apply-rhythm) (delete-from-id-context name))
        (rhythm->holes) (fill-holes chord)))))

(realize (namespace-provide-realizer) (reflect-art-definitions))
