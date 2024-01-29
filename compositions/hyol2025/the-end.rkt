#lang racket

(require (except-in tonart staff-realizer) "piece.rkt" "lib.rkt" "chuck.rkt")

;; pre-send the sound so they end up more in sync
(!send score (note->midi) (dilate 2))

(define-art ppt score (new-slide->slideshow-code))

#|
(!rktt ppt) (-> 4)


(chord c 0 [M])
(verify-chords)

(require "lib.rkt" "lecture2.rkt" (prefix-in ss: slideshow))
|#
