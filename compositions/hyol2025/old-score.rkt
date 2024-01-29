#lang racket

(module form music-lang/lib/music
  (chord-names
   I [0 4 7]
   I [0 4]
   V7/IV [0 4 7 10]
   ii [2 3 7]
   ii6 [5 4 9]
   V [7 4 7]
   V7 [7 4 7 10]
   V/V [2 4 7]
   iii [4 3 7]
   I6 [4 3 8]
   I6 [4 8]
   viio [11 3 6]
   viio6 [2 3 9]
   vi [9 3 7]
   IV [5 4 7])

  (progressions
   (I (ii I iii ii6 viio6 vi V/V))
   (vi V/V)
   (V (I V7/IV V I))
   (ii V)
   (I6 (I6 ii6))
   (V/V V)
   (iii V)
   (V7/IV IV)
   (viio6 I6)
   (ii6 V)
   (V7 I))

  (pivots
   (V/V V)
   (V7/IV V7))

  (phrases
   (2 C (cadence G ((V I))))
   (2 G (cadence C ((V7 I) (V I))))))

(module score (submod ".." form)
  (voice C 4 4 (E5 D5    C5                D5) (D5 E5 F#5 G5) (G5 F#5 F#5 F5)  (E4   D5  D5 C5))
  (voice C 4 4 (G4 F4    (1/8 E4) (1/8 C4) A4) (G4 G4 A4  B4) (B4 B4  A4  B4)  (C5   A4  B4 G4))
  (voice C 4 4 (C4 B3    C4                A3) (B3 C4 D4  D4) (D4 D4  D4  D4)  (E4   F4  D4 E4))
  (voice C 4 4 (C3 D3    E3                F3) (G3 C3 D3  G3) (G3 B2  D3  G3)  (C3   F3  G3 C3))
  ;; C major    I  viio6 I6                ii6  V  I  V/V                 V7    I    ii6 V  I
  ;; G major
)

(require 'score)
(provide (all-from-out 'score))