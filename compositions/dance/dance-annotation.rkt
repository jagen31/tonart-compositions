#lang racket
(require 2htdp/image)

(define (arm-at hour colour)
    (rotate ( * -30 hour)
            (line 0 90 colour)))

;; TODO baezantine fix thicknesses 
(define (make-body orientation)
    (define (left-body)
      (beside (square 30 'solid 'gray) (rectangle 60 120 'solid 'black)))
    (match orientation
      ['front (ellipse 100 200 'solid 'black)]
      ['back (ellipse 100 200 'outline 'black)]
      ['left (left-body)]
      ['right (flip-horizontal (left-body))]))

 ;; TODO overlay stuff 
 (define (make-dancer left-hour right-hour orientation)
    (define left-arm (arm-at left-hour 'green))
    (define right-arm (arm-at right-hour 'blue))
    (define body (make-body orientation))
    (match orientation
      ['front (beside right-arm body left-arm)]
      ['back (beside left-arm body right-arm)]
      ['left (flip-horizontal (overlay right-arm body left-arm))]
      ['right (flip-horizontal (overlay left-arm body right-arm))]))