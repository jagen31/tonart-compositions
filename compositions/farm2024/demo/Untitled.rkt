#lang racket

(require tonart (prefix-in im: 2htdp/image) (for-syntax syntax/parse))

(define-art-object (triangle []))

(define-drawer draw-triangle
  (λ (stx)
    (define/syntax-parse ([vl1 vl2 vl3] [el1 el2 el3])
      (syntax-parse stx
        [(triangle) #'([(context (string)) (context) (context)] [(context) (context) (context)])]
        [(triangle vl el) #'(vl el)]))
    #`(im:underlay/align
       'middle 'top
       (im:triangle (min #,(drawer-width) #,(drawer-height)) 'solid 'purple)
       #,(parameterize ([drawer-width (/ (drawer-width) 3)]
                        [drawer-height (/ (drawer-height) 3)])
           (println #'vl1)
           (drawer-recur #'vl1)))))

(register-drawer! triangle draw-triangle)

(define-mapping-rewriter (label-triangle-vertices [(: t triangle)])
  (λ (stx t)
    (syntax-parse stx
      [(_ label1 label2 label3)
       (syntax-parse t
         [(triangle)
          #'(triangle [label1 label2 label3] [(context (string)) (context (string)) (context (string))])]
         [(triangle _ edge-label)
          #'(triangle [label1 label2 label3] edge-labels)])])))
