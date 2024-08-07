#lang racket

(require tonart tonart/linuxsampler)


(define sound 
  (realize (linuxsampler-realizer)
    (voice@ (staff3)

      (instrument bourdon16)
      (instrument bourdon8)

      (i@ [2/3 32/3] (loop 1 (rhythm 1/3 1/3)))
      (-- [1/3 (note g 0 3)]  [1/3]
          [1 (seq (notes (b -1 3) (d 0 4)))] [1 (seq (notes (d 0 3) (g 0 3)))]
          [1 (seq (notes (b -1 3) (d 0 4)))] [1 (seq (notes (d 0 3) (g 0 3)))]
          [1 (seq (notes (b -1 3) (d 0 4)))] [1 (seq (notes (d 0 3) (f 0 3)))]
          [1 (seq (notes (b -1 3) (d 0 4)))] [1 (seq (notes (d 0 3) (e -1 3)))]
          [1 (seq (notes (g 0 3) (d 0 4)))] [1 (seq (notes (d 0 3) (e -1 3)))])
      (expand-loop)
      (apply-rhythm))

    (i@ 4
      (voice@ (staff1)
        (instrument oboe8)
        (instrument nuit8)

        (seq (notes (d 0 5) (c 0 5) (d 0 5) (g 0 5) (f 0 5) (g 0 5) (f 0 5) (e -1 5) (f 0 5)
                    (d 0 5) (c 0 5) (d 0 5) (f 0 5) (d 0 5) (c 0 5)))
        (rhythm 1/3 1/3 1/3 2/3 1/3 6/12 2/12 1/3 2/3 2/3 1/3 1/3 2/3 1/3 5/3)
        (apply-rhythm)))

    (note->midi)
    (midi->full-midi)

    (metric-interval->interval)

    #;(tempo 132/3)

    (apply-tempo)
    (d/dt)

    (instrument-map 
      [flute8 . 000/000_Montre_8]
      [oboe8 . 001/053_Hautbois_8]
      [nuit8 . 000/108_Echo_Flutes]
      [bourdon8 . 000/051_Gedackt_8]
      [bourdon16 . 000/015_Bourdon_16])))

(define file (open-output-file "sicilienne.cpp" 
                                   #:exists 'replace))
(displayln sound file)
(close-output-port file)