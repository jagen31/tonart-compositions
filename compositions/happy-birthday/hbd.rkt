#lang racket

(require tonart)

;; Happy Birthday Variations by Anne Ku

;; [ anne's happy bday extends the hold at the name ]
(define-art annes-rhythm
  happy-birthday-^s happy-birthday-rhythm (rhythm->holes) 
  ;; perform the extension
  (i@ 17 (translate 2)) (unapply-rhythm hole) (delete seq))

;; the first melody, used for a few variations.
(define-art melody-in-3 annes-rhythm happy-birthday-^s (apply-rhythm) (octave 4))

(define-art melody-in-4
  ;; FIXME jagen figure out something direct, like the following (which doesnt work.):
  ;; melody-1 (i@ [0 32] (loop 4 (i@ (1 2) (dilate 2))
  (rhythm 0.75 0.25 2 1 1 3 0.75 0.25 2 1 1 3 0.75 0.25 2 1 1 2 5 0.75 0.25 2 1 1 3)
  happy-birthday-^s (apply-rhythm) (octave 4))

(define-art melody-in-6
  ;; FIXME jagen figure out something direct, like the following (which doesnt work.):
  (rhythm 2/3 1/3 1 2 1 3 2/3 1/3 1 2 1 3 2/3 1/3 1 2 1 1 2 2/3 1/3 1 2 1 4)
  happy-birthday-^s (apply-rhythm) (dilate 3/2) (octave 4))


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

;; harmonic-rhy-in-4
(define-art harm-rhy-in-4 (rhythm 4 4 4 4 4 4 4 3 1 4))

;; voiced harmony, as chords in the harmonic rhythm
(define-art rhythmic-voiced-chords-in-4
  voiced-chords
  harm-rhy-in-4 (apply-rhythm) (voiced-chord->note-seq))

(define-art alberti-chords 
  rhythmic-voiced-chords-in-4
  (i@ [0 36] (loop 2 (-- [0.5 (! 0)] [0.5 (! 2)] [0.5 (! 1)] [0.5 (! 2)])) (expand-loop))
  (seq-ref))

(define-art march-chords
  rhythmic-voiced-chords-in-4
  (i@ [0 36] (loop 4 (! 0) (! 1) (! 2) (rhythm 1 1 2) (rhythm->holes) (fill-holes !)) (expand-loop))
  ;; the above formulation spans the harmonic rhythm wrong in measure 8
  ;; FIXME jagen use a metric interval, for clarity
  (i@ [30 32] (delete !) (loop 1 (! 0) (! 1) (! 2)) (expand-loop))
  (seq-ref))

(define-art calypso-accomp
  rhythmic-voiced-chords-in-4
  (i@ [0 36] (loop 4 (-- [1.5 (! 0)] [1.5 (! 2)] [1 (! 0)])) (expand-loop))
  (seq-ref))

;; harmonic rhythm in 6
;; FIXME about the 3 3- this is a kludge, really she's deleted the pause from
;; this variation, so I should delete the corresponding chord from the voicing.
;; 3 3 just makes it be 10 chords, writing it as 6 would only produce 9
(define-art harm-rhy-in-6 (rhythm 6 6 6 6 6 3 3 4.5 1.5 6))

;; voiced harmony, as chords in the harmonic rhythm
(define-art rhythmic-voiced-chords-in-6
  voiced-chords
  harm-rhy-in-6 (apply-rhythm) (voiced-chord->note-seq))

(define-art ballad-accomp
  rhythmic-voiced-chords-in-6
  (i@ [0 48] (loop 3 (seq (ix-- (! 0) (! 1) (! 2) (! 0) (! 2) (! 1))) (urhy 0.5)
                     (i@ [1.5 2] (transpose-diatonic 7))) (expand-loop))
  (apply-rhythm) (seq-ref) (key c 0 major) (note->^o) (^o->^ 3) #;(run-transpose-diatonic))

;; ---------------

;; assemble the variations

(define-art bass-line-1
  (seq (^s 8 5 5 8 8 4 4 8 7 8)) (rhythm* 3 3 3 3 3 3 3 2 1 3) (translate 1) (octave 2))

(define-art bass-var (voice@ (melody) melody-in-3) (voice@ (accomp) bass-line-1))
(define-art block-chords-var (voice@ (melody) melody-in-3) (voice@ (accomp) block-chords (translate 1)))
(define-art broken-chords-var (voice@ (melody) melody-in-3) (voice@ (accomp) broken-chords (translate 1)))
(define-art waltz-var (voice@ (melody) melody-in-3) (voice@ (accomp) waltz-chords (translate 1)))
(define-art alberti-bass-var (voice@ (melody) melody-in-4) (voice@ (accomp) alberti-chords (translate 1)))
(define-art march-var (voice@ (melody) melody-in-4) (voice@ (accomp) march-chords (translate 1)))
(define-art calypso-var (voice@ (melody) melody-in-4) (voice@ (accomp) calypso-accomp (translate 1)))
(define-art ballad-var (voice@ (melody) melody-in-6) (voice@ (accomp) ballad-accomp (translate 1.5)))


(provide (all-defined-out))
