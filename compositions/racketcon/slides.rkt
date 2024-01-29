#lang slideshow

(require slideshow/text)

(set-page-numbers-visible! #f)

;; Background parameters
(define background-image (make-parameter #f))
(define (background-image-pict)
   (define bg (background-image))
   (inset (scale bg (/ 1024 (pict-width bg)) (/ 768 (pict-height bg)))
          (- margin)))

;; Slide number parameters
(define slide-number (make-parameter 0))
(define (add1-slide-number) (slide-number (add1 (slide-number))))
(define format-slide-number
   (make-parameter
    (lambda (num)
      (scale (rt (number->string (slide-number))) 3/5))))

;; Slide assembly

(define (add-slide bg-pct pct)
   (refocus (ct-superimpose bg-pct pct) bg-pct))

(define (add-slide-number pct)
   (refocus
    (rb-superimpose pct ((format-slide-number) (slide-number)))
    pct))

(current-slide-assembler
  (let ([orig  (current-slide-assembler)])
    (lambda (title sep body)
      (let* ([pct  (if (background-image)
                       (background-image-pict)
                       (inset (blank 1024 768) (- margin)))]
             [pct  (add-slide pct (orig title sep body))]
             [pct  (if (slide-number) (add-slide-number pct) pct)])
        pct))))


(current-title-color "white")
(background-image (scale (bitmap "images/mystery.jpg") 1/4))
(slide #:title "((λ (x) (x x)) (λ (x) (x x)))")

(current-title-color "white")
(background-image (filled-rectangle (get-client-w) (get-client-h) #:color "red"))

(add1-slide-number)
(slide #:title "Great Composers Steal: Obbligato Reuse of Racket in Tonart"
  (scale (bitmap "images/logo.png") 1/4))
(current-title-color "black")
(background-image (filled-rectangle (get-client-w) (get-client-h) #:color "white"))
(add1-slide-number)
(slide 
  #:gap-size 10
  (para #:align 'left (t "\"Good composers borrow, great ones steal!\""))
  (para #:align 'left (italic (t "-Igor Stravinsky")))
  (scale (bitmap "images/stravinsky.jpg") 1/7)
  (subscript (t "Taken by George Grantham Bain's news picture agency.")))
(add1-slide-number)
(slide
  #:gap-size 10
  (para #:align 'left (t "\"If you're a good composer, you steal good steals\""))
  (para #:align 'left (italic (t "-Leonard Bernstein")))
  (scale (bitmap "images/bernstein.jpg") 1/8)
  (subscript (t "Al Ravenna, World Telegram staff photographer - Library of Congress.")))
(add1-slide-number)
(slide 
  #:gap-size 10
  (para #:align 'left 
    (scale (bitmap "images/obbligato1.png") 1/3)
    (t "Obbligato - an instrumental part, typically distinctive in effect,") 
    (t "which is integral to a piece of music and should not be omitted in")
    (t "performance.")
    (scale (bitmap "images/obbligato2.png") 1/3)))