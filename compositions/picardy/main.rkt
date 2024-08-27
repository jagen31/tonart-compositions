#lang racket

(require art tonart "picardy.rkt")

(define-art the-piece-rsound
  the-piece
  (x-picardy-tune)
  (metric-interval->interval)

  (voice@ (one) (instrument Clarinet)) 
  (voice@ (two) (instrument |Yamaha Grand Piano|)) 
  (voice@ (three) (instrument Clarinet))
  (voice@ (four) (instrument Clarinet))
  (voice@ (five) (instrument Clarinet))

  (key d 0 minor) (^->note) (tuning 12tet) (note->tone) 
  (volume 5)

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


#;(define-art the-piece-mxml
  the-piece
  (x-picardy-tune)
  (metric-interval->interval) 
  (key d 0 minor) (^->note) 

  (time-sig 4 4)
  (enclose-in-measures [note])
  (rewrite-in-seq 
    (insert-rests)
    (measure->mxml-measure) #:capture [time-sig]))

(define-art-rewriter chuck-postlude
  (lambda (stx)
    #'(context 
        (x-picardy-tune) 
        (sound-map) 
        (^->note) (note->midi)  (volume 5) 
        (voice@ (one) (channel 1)) (voice@ (two) (channel 2)) 
        (voice@ (three) (channel 3)) 
        (voice@ (four) (channel 1)) (voice@ (five) (channel 1)) 
        (midi->full-midi) (tempo 120) (apply-tempo) (d/dt))))

#;(realize (draw-realizer [800 200]) (music the-piece))

#;(rs-write (realize (music-rsound-realizer) the-piece-rsound) "picardy.wav")

#;(displayln (realize (linuxsampler-realizer) the-piece-lsampler)
           (open-output-file "picardy.cpp" #:exists 'replace))

#;(write-xml (realize (unload-musicxml) the-piece-mxml)
           (open-output-file "picardy.musicxml" #:exists 'replace))
