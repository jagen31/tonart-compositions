#lang racket

(require tonart)

;; Happy Birthday Variations by Anne Ku

;; [ anne's happy bday extends the hold at the name ]
(define-art annes-rhythm
  (happy-birthday) (happy-birthday->rhythm) (rhythm->holes) 
  ;; perform the extension
  (i@ 17 (translate 2)) (unapply-rhythm hole) (delete seq))

;; the first melody, used for a few variations.
(define-art melody-1 annes-rhythm (happy-birthday) (happy-birthday->^s) (apply-rhythm) (octave 4))

;; ------------------------

;; We'll define the voicing used throughout the whole piece

;; harmony
(define-art harmony
  (seq (chords [c 0 M] [g 0 M 7] [g 0 M 7] [c 0 M] 
               [c 0 M 7] [f 0 M] [f 0 M] [c 0 M] [g 0 M 7] [c 0 M])))
;; harmonic-rhythm
(define-art harm-rhy (rhythm 3 3 3 3 3 3 3 2 1 3))

;; voiced harmony, as a sequence of chords
(define-art voiced-chords 
  harmony
  (rewrite-in-seq 
    (chord->voiced-chord 3) 
    (ix@ 0 (seq (notes [c 0 3] [e 0 3] [g 0 3])) (fill-harmony)) (voice-lead 3)))

;; voiced harmony, as chords in the harmonic rhythm
(define-art rhythmic-voiced-chords
  voiced-chords
  harm-rhy (apply-rhythm) (voiced-chord->note-seq))

;;-----------------------

;; define some rhythmic variations over the voice leading
(define-art block-chords
  rhythmic-voiced-chords
  harm-rhy (rhythm->holes) (! 0) (! 1) (! 2) (fill-holes !) (seq-ref))

(define-art broken-chords
  rhythmic-voiced-chords
  (i@ [0 27] (loop 3 (-- [1 (! 0)] [1 (! 1)] [1 (! 2)])) (expand-loop))
  (seq-ref))

(define-art waltz-chords
  rhythmic-voiced-chords
  (i@ [0 27] (loop 3 (-- [1 (! 0)] [1 (! 1) (! 2)] [1 (! 1) (! 2)])) (expand-loop))
  (seq-ref))

;; ---------------

;; assemble the variations

(define-art bass-line-1
  (seq (^s 8 5 5 8 8 4 4 8 7 8)) (rhythm* 3 3 3 3 3 3 3 2 1 3) (translate 1) (octave 2))

(define-art bass-var (voice@ (melody) melody-1) (voice@ (accomp) bass-line-1))
(define-art block-chords-var (voice@ (melody) melody-1) (voice@ (accomp) block-chords (translate 1)))
(define-art broken-chords-var (voice@ (melody) melody-1) (voice@ (accomp) broken-chords (translate 1)))
(define-art waltz-chords-var (voice@ (melody) melody-1) (voice@ (accomp) waltz-chords (translate 1)))

(provide (all-defined-out))
