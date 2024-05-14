#lang racket

(require tonart "wolfenstein.rkt")

(define-art 
  wolf-tones song (^->note) (tuning 12tet) (note->tone) 
  (voice@ (melody) (volume 8)) 
  (voice@ (mid-bass) (volume 5)) (voice@ (bass) (volume 5)) 
  (voice@ (accomp) (volume 2))
  (tempo 144) (apply-tempo))

#;(tyr wolf-tones (compute-type))
(rs-write (mrsr wolf-tones) "wolf.wav")