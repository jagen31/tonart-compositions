#lang racket
(require 2htdp/image)

(define (the-pen color)
  (pen color 8 "solid" "round" "bevel"))

(define (arm-at hour colour)
    (rotate (* -30 hour)
            (put-pinhole 0 110 (line 0 110 (the-pen colour)))))

(define (make-body orientation)
    (define (left-body)
      (beside (square 30 'solid 'yellow) (rectangle 30 120 'solid 'yellow) (rectangle 30 120 'solid (color 130 0 200 255))))
    (match orientation
      ['towards (ellipse 100 150 'solid 'yellow)]
      ['away (ellipse 100 150 'solid (color 130 0 200 210))]
      ['left (left-body)]
      ['right (flip-horizontal (left-body))]))

 (define (make-dancer left-hour right-hour orientation)
   (define left-arm (arm-at left-hour 'green))
   (define right-arm (arm-at right-hour 'blue))
   (define body (make-body orientation))
   
   (clear-pinhole
    (match orientation
      ['towards
       (define one-arm (overlay/pinhole right-arm (put-pinhole 10 75 body)))
       (underlay/pinhole (put-pinhole (+ (pinhole-x one-arm) 80) (pinhole-y one-arm) one-arm) left-arm)]
      ['away
       (define one-arm (underlay/pinhole left-arm (put-pinhole 10 75 body)))
       (overlay/pinhole (put-pinhole (+ (pinhole-x one-arm) 80) (pinhole-y one-arm) one-arm) right-arm)]
      ['left
       (define one-arm (overlay/pinhole left-arm (put-pinhole 40 50 body)))
       (overlay/pinhole (put-pinhole (- (pinhole-x one-arm) 5) (pinhole-y one-arm) one-arm) right-arm)]
      ['right
       (define one-arm (overlay/pinhole right-arm (put-pinhole 40 50 body)))
       (overlay/pinhole (put-pinhole (- (pinhole-x one-arm) 5) (pinhole-y one-arm) one-arm) left-arm)])))

#|
(for*/list ([o '(towards away left right)] [i (in-range 12)])
  (make-dancer i i o))
|#

(provide (all-defined-out))
