#lang racket

(require art art/sequence racket/system racket/runtime-path art/timeline art/namespace tonart tonart/common-practice art/coordinate/subset 
         (for-syntax racket/list tonart/liszt racket/dict racket/set racket/match syntax/parse racket/syntax syntax/id-table))

(define-art-rewriter musicxml->tonart
  (λ (stx)
    (qq-art stx
      (context
       (rewrite-in-seq
        (rewrite-in-mxml-measure
         (rewrite-in-seq (extract-mxml-chords)))
        (mxml-measure->measure)
        (rewrite-in-measure (durations->intervals) (ungroup-notes))
        (measure->music))
       (inline-music-seq) (unsubdivide) (delete-from-id-context voice)))))

;; define some sample music
(define-art bass-motif
  (music
   (seq (notes (a 0 2) (b 0 2) (c 0 3) (d 0 3)))
   (rhythm 1 2 3 2)
   (apply-rhythm)))

(define-art melody
  (music (picardy) (picardy->rhythm) (seq (picardy) (picardy->^s)) (apply-rhythm)
         (key a 0 minor) (octave 5) (^->note) (delete key)))

(define-art accomp
  (music
   (load-musicxml "test.musicxml" [_])
   (musicxml->tonart)))

(define-art song
  (music (voice@ (bass) (i@ [0 96] (loop 8 (m! bass-motif))) (expand-loop))
         (voice@ (accomp) (i@ [0 96] (loop 16 (m! accomp))) (expand-loop))
         (voice@ (mel) (m! melody))))

;;-----------------

; 1. reflect definitions into namespace art
(define-art reflected (reflect-art-definitions))
(realize (draw-realizer [1200 200]) (namespace reflected))
; 2. rewrite in all names at once
(define-art rewritten reflected (key a 0 minor) (rewrite-in-music (note->^o)))
(realize (draw-realizer [1200 200]) (namespace rewritten))
; 3. reify back into definitions and provide
(realize (namespace-provide-realizer) rewritten)
