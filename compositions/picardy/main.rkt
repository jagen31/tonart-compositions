#lang racket

(require tonart)

(define-simple-rewriter picardy-tune x-picardy-tune
  (picardy) (picardy->rhythm) (seq (picardy) (picardy->^s)) (apply-rhythm))

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
      (measure@ [1 12] (loop 24 countermelody) (metric-interval->interval) (expand-loop) (octave 5)))

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


#;(define-art the-piece-rsound
  the-piece
  (x-picardy-tune)
  (metric-interval->interval)

  (voice@ (one) (instrument Clarinet)) 
  (voice@ (two) (instrument |Yamaha Grand Piano|)) 
  (voice@ (three) (instrument Clarinet))
  (voice@ (four) (instrument Clarinet))
  (voice@ (five) (instrument Clarinet))

  (key d 0 minor) (^->note) (note->midi) 

  (tempo 120) (apply-tempo))

(define-art the-piece-lsampler
  the-piece
  (x-picardy-tune)
  (metric-interval->interval)

  (voice@ (one) (instrument flute)) 
  (voice@ (two) (instrument viola)) 
  (voice@ (three) (instrument pedal))
  (voice@ (four) (instrument viola))
  (voice@ (five) (instrument viola))

  (instrument-map [flute . 001/005_Flute_harmonique] [viola . 000/020_Viola_8_Flute_4]
                  [pedal . |000/112_Prin.(ped.)16+8|])

  (key d 0 minor) (^->note) (note->midi) (midi->full-midi)
  (tempo 120) (apply-tempo) (d/dt))


(define-art the-piece-mxml
  the-piece
  (x-picardy-tune)
  (metric-interval->interval) 
  (key d 0 minor) (^->note) 

  (time-sig 4 4)
  (enclose-in-measures [note])
  (rewrite-in-seq 
    (insert-rests)
    (measure->mxml-measure) #:capture [time-sig]))

(realize (draw-realizer [800 200]) (music the-piece))

#;(rs-write (realize (music-rsound-realizer) the-piece-rsound) "picardy.wav")

(displayln (realize (linuxsampler-realizer) the-piece-lsampler)
           (open-output-file "picardy.cpp" #:exists 'replace))

(write-xml (realize (unload-musicxml) the-piece-mxml)
           (open-output-file "picardy.musicxml" #:exists 'replace))
