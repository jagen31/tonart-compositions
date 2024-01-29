#lang racket

(require (except-in tonart staff-realizer) "lib.rkt" "chuck.rkt")

(define-art folksy-^s (seq (^s 1 -2 1 -2 -2 -2 1 -2 1 -2 -2 -2 1 -2 -2 1 2 5)))
(define-art folksy-rhythm (rhythm 1 3/4 1/4 1 3/4 1/4 1 3/4 1/4 1 3/4 1/4 1 3/4 1/4 1 1 3))
(define-art folksy- folksy-^s folksy-rhythm (apply-rhythm))

(define-art accomp-^s (seq (^s 1 0 1 0 1 2 3)))
(define-art accomp-harm-rhythm (rhythm 2 2 2 2 2 2 4))
(define-art accomp-cell-rhythm (rhythm 3/2 1/2))

(define-art folksy-accomp1
  accomp-^s accomp-harm-rhythm (apply-rhythm)
  (i@ [0 15] (loop 2 accomp-cell-rhythm)) (expand-loop)
  (rhythm->holes)
  (fill-holes ^))

(define-art folksy-accomp2
  folksy-accomp1 (transpose-diatonic 2) (run-transpose-diatonic))

(define-art folksy-accomp folksy-accomp1 folksy-accomp2)

(define-art folksy
  (seq (music (voice@ [soprano] folksy-) (voice@ [accomp] folksy-accomp) (key f 0 minor))
       (music (voice@ [soprano] folksy-) (voice@ [accomp] folksy-accomp)
              (key d -1 major) (transpose-diatonic 4) (run-transpose-diatonic)))
  (inline-music-seq)
  (voice@ [soprano] (octave 5))
  (voice@ [accomp] (octave 4)))


#|

<<Scratchpad>>


Change the rhythm of the accomp

(define-art accomp-cell-rhythm (rhythm 1 1))


PLay it

(!play
   folksy
   (^->note) (note->midi))

Delete 4 beats

(!play
   folksy
   (voice@ [soprano] (i@ [20 22] (delete ^)))
   (^->note) (note->midi))


Transpose 4 beats

(!play
   folksy
   (i@ [4 8] (transpose-diatonic 4) (run-transpose-diatonic))
   (^->note) (note->midi))


Play with just folksy template

(!play
   (voice@ [accomp] folksy- (octave 5))
   (voice@ [soprano] folksy-accomp (octave 4))
   (key f 0 minor)
   (^->note) (note->midi))

Change around the folksy-^
(seq (^s 1 -2 1 -2 -2 -2 1 -2 1 -2 -2 -2 1 -2 -2 1 2 5))

[original]==

(seq (^s 1 0 1 0 -2 0 1 0 1 0 -2 0 1 0 -2 1 2 3))
|#