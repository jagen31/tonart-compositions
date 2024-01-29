#lang racket

(require qi 2htdp/image)

(define OUTER-RADIUS 120)
(define INNER-RADIUS 105)
(define-values (EYE-WIDTH EYE-HEIGHT EYE-X-OFF) (values 25 25 40))

(define PRIMARY 'cornflowerblue)
(define ACCENT 'white)
(define (PEN color) (pen color 13 "solid" "round" "bevel"))

(~> [PRIMARY ACCENT]
    (~>
     (-< (~> (select 1 2) ▽) (~> (select 2 1) ▽)) ; duality
     (>< (~> △
             (-< (gen (curry wedge INNER-RADIUS 180 'solid)) _)
             (-< (~> (block 2) apply (scale 1/2 _)) (~> (block 3) apply) 2> 3>) ; 2 circles
             (-< 3> 4> (~> (select 1 2) (overlay/align 'left 'bottom _ _))) ; compose
             (-< (gen EYE-WIDTH EYE-HEIGHT) _)
             (-< 5>
                 (~> (block 4 5) (== _ + (PEN _)) line) (gen (- EYE-X-OFF))
                 (~> (block 3 5) (== _ - (PEN _)) line) (gen (+ (* -2 INNER-RADIUS) EYE-X-OFF EYE-WIDTH)))
             (-< (~> 1> (as image)) (~> (select 2 3) ▽) (~> (select 4 5) ▽)) ; eyes with x positions
             (<< (~> (== △ _) (-< (gen 'left 'bottom) 1> 2> (gen 0) 3>) overlay/align/offset) ; teeny fold
                 ;; FIXME jagen trouble with the init arg- why cant it receive the inputs?
                 (gen image))))))
    ;; the punchline
    (== _ (rotate 180 _)) above
    (overlay _ (circle OUTER-RADIUS 'solid PRIMARY)))