#lang slideshow

(require slideshow slideshow/text)

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
(background-image (filled-rectangle (get-client-w) (get-client-h) #:color "red"))
(slide #:title "How to Score in the Art Racket"
  (scale (bitmap "images/logo.png") 1/4))
(current-title-color "black")
(background-image (filled-rectangle (get-client-w) (get-client-h) #:color "white"))
(slide 
  #:gap-size 50
  (para #:align 'left (t "Questions?")))