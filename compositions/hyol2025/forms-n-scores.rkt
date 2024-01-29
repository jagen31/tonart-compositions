#lang racket

(require art "lib.rkt")
(provide (all-defined-out))

(define-art forms
  (ix--
    (rkt
      (module form music-lang/lib/music
        (chord-names
          I [0 4 7]
          V [7 4 7])
        (progressions
          (I [I V])
          (V [I V]))
        (pivots)
        (phrases)))
    (rkt
      (module form music-lang/lib/music
        (chord-names
          I [0 4 7]
          IV [5 4 7]
          V [7 4 7])
        (progressions
          (I [I IV V])
          (IV [I IV V])
          (V [I V]))
        (pivots)
        (phrases)))
    (rkt
      (module form music-lang/lib/music
        (chord-names
          I [0 4 7]
          IV [5 4 7]
          V [7 4 7]
          V/V [2 4 7])
        (progressions
          (I [I IV V V/V])
          (IV [I IV V V/V])
          (V [I V])
          (V/V [V]))
        (pivots
         [V/V V])
        (phrases)))))

(define-art scores
  (ix--
    (rkt
      (module score (submod ".." form)
        (voice C 4 4 (E5 D5 E5 D5))
        (voice C 4 4 (G4 G4 G4 G4))
        (voice C 4 4 (C4 B3 C4 B3))
        (voice C 4 4 (C3 G2 C3 G2))
        ;;            I  V  I  V 
        ))
    (rkt
     (module score (submod ".." form)
       (voice C 4 4 (E5 F5 E5 D5))
       (voice C 4 4 (G4 A4 G4 G4))
       (voice C 4 4 (C4 C4 C4 B3))
       (voice C 4 4 (C3 F2 C3 G2))
       ;;            I  IV  I  V 
       ))
    (rkt
     (module score (submod ".." form)
       (voice C 4 4 (E5 F5 F#5 G5) ((1/2 E5) (1/2 D5)))
       (voice C 4 4 (G4 A4 A4  D4) ((1/2 C#4) (1/2 A4)))
       (voice C 4 4 (C4 C4 A3  B3) ((1/2 A3)  (1/2 F#3)))
       (voice C 4 4 (C3 F2 D3  G2) ((1/2 A2)  (1/2 D2)))
       ;;         C: I  IV V/V V
       ;;               G: V   I     V/V     V
       ))))



#|
<<scratchpad>>

show it

(show [2000 1200] (seq (ix-- forms scores)))
|#