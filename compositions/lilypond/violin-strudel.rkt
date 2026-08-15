#lang racket

;; Compile the World's Smallest Violin theme (sample 9 in samples.rkt)
;; to a Strudel pattern via music-strudel-realizer.

(require tonart (except-in "../strudel/strudel.rkt" dynamic crescendo))

(define-art violin-loop
  (voice@ (violin)
    (loop 16
      (i@ [0 16]
        (instrument "sax")
        (seq (note b 0 4) (note e 0 4)
             (note c 0 5) (note b 0 4) (note a 0 4) (note g 0 4)
             (note f 1 4) (note d 0 4) (note d 0 5) (note c 0 5)
             (note b 0 4))
        (rhythm 2 2   3/2 1/2 3/2 1/2   1 1 3/2 1/2   4)
        (apply-rhythm)))))

(define out
  (build-path (path-only (variable-reference->module-source
                          (#%variable-reference)))
              "09-smallest-violin.strudel"))

(call-with-output-file out #:exists 'replace
  (λ (p) (display (realize (music-strudel-realizer) violin-loop) p)))

(printf ";; wrote ~a\n" out)
