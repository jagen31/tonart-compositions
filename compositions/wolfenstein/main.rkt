#lang racket

(require tonart art/sequence/ravel)

;; FIXME 0.5 0.5 0.5 ... figure that out
(define-art cello-suite-thing (seq (^s 1 5 5 5 9 5 9 5 1)) (rhythm 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 1) (apply-rhythm))

(define-art bass-rhythm (rhythm 6.5 0.5 0.5 0.5))

(define-art bassline
  (-- [8 bass-rhythm (seq (^s 1 [0 1] 0 [-1 1]))] [8 bass-rhythm (seq (^s -1 [-1 1] 0 [0 1]))]) (apply-rhythm))

(define-art melody-rhythm (rhythm 1 7))

(define-art melody1
  (i@ [0 32]
      ;; FIXME jagen needs split
      (seq (ix-- (seq (^s -2 2)) (seq (^s 3 -1)) (seq (^s -2 2)) (seq (^s 3 4)))) #;(run-apl (split *ctxt*)) (uniform-rhythm 8) (apply-rhythm)
      (loop 8 melody-rhythm) (expand-loop) (apply-rhythm)))

(define-art melody2
  (i@ [0 32]
      ;; FIXME jagen needs split here too
      (seq (ix-- (seq (^s 1 5)) (seq (^s 6 3)) (seq (^s 1 5 6)) (seq (^s 5 4 3 2)))) #;(run-apl (split *ctxt*)) (uniform-rhythm 8) (apply-rhythm)
      (-- [8 melody-rhythm] [8 melody-rhythm] [8 (rhythm 1 6 1)] [8 (rhythm 1 3 1 3)]) (apply-rhythm)))

(define-art cello-intro
  (i@ [0 32] (loop 8 cello-suite-thing) (expand-loop)
    (seq (ix-- (key d 0 minor) (key b -1 major) (key d 0 minor) (key b -1 major)))
    (uniform-rhythm 8) (apply-rhythm) 
    (-- [8 (octave 4)] [8 (octave 3)] [8 (octave 4)] [8 (octave 3)])))

(define-art cello-verse
  (i@ [0 64] (loop 8 cello-suite-thing) (expand-loop)
    ;; FIXME shorten with apl once concat works
    ;; also maybe use chords + voice leading?
    (seq (ix--
           (key d 0 minor) (key b -1 major) (key d 0 minor) (key b -1 major) 
           (key g 0 minor) (key b -1 major) (key g 0 minor) (key a 0 major)))
    (uniform-rhythm 8) (apply-rhythm) 
    ;; Unfortunate!
    (-- [8 (octave 4)] [8 (octave 3)] [8 (octave 4)] [40 (octave 3)])))
    
(define-art bass-intro (i@ [0 32] (loop 16 bassline)) (expand-loop) (key d 0 minor) (octave 4))

(define-art bass-verse
  (i@ [0 32] (loop 16 bassline)) (expand-loop)
    (measure@ 9 (-- [8 (^ -3)] [8 (^ -1)] [8 (^ -3)] [8 (^ -2)])) (metric-interval->interval)
    (key d 0 minor) (octave 4))

;; FIXME jagen abstract?
;; FIXME jagen add augmented 6
(define-art harmony-dm
  (chords [d 0 m] [b -1 M] [d 0 m] [b -1 M [aug 6]] [g 0 m] [b -1 M [aug 6]] [g 0 m] [a 0 major]))

(define-art voice-lead-harmony1
  harmony-dm (chord->voiced-chord 3) (seq (notes [d 0 4] [f 0 4] [a 0 4])) (fill-harmony 0) (voice-lead 3))

(define-art voice-lead-harmony2
  harmony-dm (chord->voiced-chord 3) (seq (notes [a 0 4] [d 0 5] [f 0 5])) (fill-harmony 0) (voice-lead 3))

(define-art-rewriter do-accomp 
  (λ (stx)
    (qq-art stx
      (context
        (uniform-rhythm 8) (apply-rhythm) (voiced-chord->note-seq) 
        (uniform-rhythm 1) (! 0) (! 1) (! 2) (rhythm->holes) (fill-holes !) (seq-ref)))))

(define-art song
  (voice@ (melody) (key d 0 minor) 
    (mi@ ([8 4]) melody1 (octave 5)) (mi@ ([16 4]) melody2 (octave 5)) 
    (mi@ ([24 4]) melody1 (octave 6)) (mi@ ([32 4]) melody2 (octave 6)) (metric-interval->interval))

  (voice@ (accomp)
    (seq 
      (ix-- (music (i@ [0 32] (music-rest))) 
        (music (seq voice-lead-harmony1) (i@ [0 64] (do-accomp))) (music (seq voice-lead-harmony2) (i@ [0 64] (do-accomp)))))
                    
    (inline-music-seq))

  (voice@ (mid-bass) (seq (ix-- (music cello-intro) (music cello-verse) (music cello-verse))) (inline-music-seq))
  ;; FIXME jagen make length aware loop here too
  (voice@ (bass) 
    (seq (ix-- (music bass-intro) (music bass-verse) (music bass-verse))) (inline-music-seq)))
