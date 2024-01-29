#lang s-exp music-lang/lib/music

(chord-names
 I [0 4 7]
 IV [5 4 7]
 V [7 4 7]
 V/V [2 4 7])

(progressions
 (I [I IV V V/V])
 (IV [IV V I V/V])
 (V [I V])
 (V/V [I V]))

(pivots
 [V/V V])

(phrases)