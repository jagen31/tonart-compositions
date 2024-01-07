#lang racket

(require art tonart tonart/rsound rsound (for-syntax syntax/parse))

(define-art moonlight-chords
  (seq (chords (c 1 m #:v [(g 1 3) (c 1 4) (e 0 4)]) 
          (a 0 M) (d 0 M) (g 1 M 7) (c 1 m) (g 1 sus 4) 
          (b 1 dim #:v [(f 1 3) _ _]) 
          (c 1 m #:v [(e 0 3) (g 1 3) _]) (c 1 m #:v [(g 1 3) (c 1 4) _])
          (g 1 sus 7) (c 1 m) (f 1 m) (e 0 M) (b 0 M 7 #:v [(a 0 3) _ _]) 
          (e 0 M) (e 0 m) (g 0 M 7)) ; [seq chord]
       (chord->voiced-chord 3)
       (voice-lead 3)) ; [seq [voiced-chord 3]])

  (rhythm 8 2 2 1 1 1 1 1 3 4 2 2 2 2 4 4 4)
  (apply-rhythm))

(define-art moonlight-accomp
  moonlight-chords
  (i@ [0 44] (loop 1 (i@ [0 1] (seq (ix-- (! 0) (! 1) (! 2))) (rhythm 1/3 1/3 1/3)))
             (expand-loop) (apply-rhythm))
  (voiced-chord->note-seq)
  (seq-ref))

(define result
  (realize (linuxsampler-realizer) (voice@ (accomp) moonlight-accomp (instrument piano))

    (voice@ (bass)
      (seq (notes (c 1 3) (b 0 2) (a 0 2) (f 1 2) (g 1 2) (c 1 3) (b 1 2) (c 1 3) (f 1 2) (b 0 2) (b 0 2) 
                  (e 0 3) (e 0 3) (d 0 3)))
      (rhythm 4 4 2 2 4 4 4 2 2 2 2 4 4 4)
      (apply-rhythm)
      (instrument pedal)
      (instrument piano))

    (voice@ (melody)
      (mi@ [(5 4) (10 4)]
        (seq (notes (g 1 4) (g 1 4) (g 1 4) (g 1 4) (g 1 4) (g 1 4) (a 0 4) (g 1 4) (f 1 4) (b 0 4) (e 0 4)))
        (rhythm 0.75 0.25 3 0.75 0.25 2 2 2 1 1 1))

      (mi@ [(10 4)]
        (seq (notes (g 0 4) (g 0 4) (g 0 4) (g 0 4) (g 0 4)))
        (rhythm 0.75 0.25 3 0.75 0.25))

      (apply-rhythm)
      (instrument flute))

    (measure@ 1
      (note->midi)
      (instrument-map 
        [piano . 000/000_Montre_8]
        [pedal . 000/000_Montre_8]
        [flute . 000/003_Montre_8_Prestant_4])
      (tempo 66)
      (metric-interval->interval)
      (apply-tempo)
      (midi->full-midi)
      (d/dt))))


(define file (open-output-file "moonlight.cpp" #:exists 'replace))
(displayln result file)
(close-output-port file)
