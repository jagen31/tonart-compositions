#lang racket

;; RECONSTRUCTED after an accidental deletion.  No MuseScore backup of
;; this tune survived, so `scores/twinkle-twinkle.musicxml` was rebuilt
;; from the notes in the paper's rendered lilypond
;; (porchfest-resources/music-5.ly) — the standard, public-domain
;; melody in D major.
;;
;; The sibling tunes are `#lang s-exp tonart/private/musicxml/lang`
;; loaders, but that lang would not bind `song` from the rebuilt score,
;; so this defines `song` directly.  `load-musicxml` resolves its path
;; against the current directory, which is not this file's directory
;; when the paper realizes the program, so `define-tune` computes the
;; absolute score path from this module's own location at compile time
;; (the same absolute-path trick the lang uses) and hands `load-musicxml`
;; a literal — a plain macro inside `define-art` would not expand.

(require tonart (for-syntax racket/base racket/path))
(provide song)

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

(define-tune song "scores/twinkle-twinkle.musicxml" [song])
