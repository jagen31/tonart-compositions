#lang racket

;; RECONSTRUCTED after an accidental deletion.  The three sibling tunes
;; are plain `#lang s-exp tonart/private/musicxml/lang` loaders that
;; provide `song`; this one instead provides the jig's two strains
;; separately (`song-a`, `song-b`), which `program.rkt` places back to
;; back in the Celebration section.  The original split logic was lost,
;; so the A/B boundary below is a best guess at the midpoint (18 bars of
;; 6/8 at divisions=2 ≈ 108 units).  VERIFY the interval against the
;; score / your DrRacket buffer.
;;
;; `load-tune` resolves the score path against this module's own
;; directory at compile time (load-musicxml otherwise resolves against
;; the current directory, which is wrong when the paper realizes the
;; program from elsewhere), handing `load-musicxml` an absolute literal.

(require tonart (for-syntax racket/base racket/path))
(provide song-a song-b)

(define-syntax (define-tune stx)
  (syntax-case stx ()
    [(_ name rel [voice ...])
     (let* ([src (syntax-source #'rel)]
            [dir (and (path? src) (path-only (path->complete-path src)))]
            [abs (if dir (path->string (build-path dir (syntax-e #'rel)))
                     (syntax-e #'rel))])
       #`(define-art name
           (load-musicxml #,abs [voice ...])
           (musicxml->tonart)))]))

(define-tune loaded "scores/swallowtail.musicxml" [one])

(define-art song-a loaded (i@ [0 54]   (zoom)))
(define-art song-b loaded (i@ [54 108] (zoom)))
