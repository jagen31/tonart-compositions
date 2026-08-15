#lang racket

;; Ten short dance fragments — each is a sequence of arm-diagrams hung
;; above a steady quarter-note pulse.  Rendered as ONE LilyPond file
;; (10-dance-fragments.ly) with one staff per fragment.

(require tonart "lilypond.rkt" "arm-diagram.rkt"
         (for-syntax racket/base syntax/parse racket/list))

(define (write-ly! filename contents)
  (define path (build-path (path-only (or (variable-reference->module-source
                                           (#%variable-reference)) "."))
                           filename))
  (call-with-output-file path #:exists 'replace
    (λ (out) (display contents out)))
  (printf ";; wrote ~a\n" filename))

;; (dance-fragment <voice-id> <facing> [l r] ...)
;;   -> a voice@ with a facing context, one quarter-rest per beat starting
;;      at t=0, and the i'th arm-diagram pinned to t=i.
;;
;; Implemented as an art-rewriter so it expands inside `define-art` bodies
;; (regular Racket macros aren't expanded there — `define-art` only
;; recognizes art objects/rewriters/coordinates).
(define-art-rewriter dance-fragment
  (λ (stx)
    (syntax-parse stx
      [(_ voice-id:id face:id [l:number r:number] ...)
       (define n (length (syntax->list #'([l r] ...))))
       (with-syntax ([(idx ...) (for/list ([i (in-range n)])
                                  (datum->syntax stx i))]
                     [(nxt ...) (for/list ([i (in-range n)])
                                  (datum->syntax stx (add1 i)))])
         (qq-art stx
           (voice@ (voice-id)
             (facing face)
             (i@ [idx nxt] (music-rest)) ...
             (i@ idx (arm-diagram l r)) ...)))])))

(define-art ten-dances
  ;; 1. Wave — arms swing up from rest, peak overhead, drift back down.
  (dance-fragment wave towards
    [6 6] [5 7] [4 8] [3 9] [12 12] [3 9] [4 8] [6 6])

  ;; 2. Clap — arms open wide, clap together, repeat.
  (dance-fragment clap towards
    [3 9] [5 7] [3 9] [5 7] [3 9] [5 7] [3 9] [5 7])

  ;; 3. Windmill — both arms rotate together a quarter-turn each beat.
  (dance-fragment windmill towards
    [12 12] [3 3] [6 6] [9 9] [12 12] [3 3] [6 6] [9 9])

  ;; 4. Disco — classic Travolta point, alternating sides.
  (dance-fragment disco right
    [12 6] [6 12] [12 6] [6 12] [12 6] [6 12] [12 6] [6 12])

  ;; 5. Jumping jacks — down-out-up-out repeating.
  (dance-fragment jumping-jacks towards
    [6 6] [3 9] [12 12] [3 9] [6 6] [3 9] [12 12] [3 9])

  ;; 6. Conductor — 4-beat conducting pattern (down-left-right-up).
  (dance-fragment conductor towards
    [6 6] [4 4] [8 8] [12 12] [6 6] [4 4] [8 8] [12 12])

  ;; 7. Funky chicken — wings flap up and down with elbows out.
  (dance-fragment funky-chicken towards
    [3 9] [4 8] [3 9] [4 8] [3 9] [4 8] [3 9] [4 8])

  ;; 8. Flossing — arms swing in opposite directions across the body.
  (dance-fragment flossing towards
    [4 8] [8 4] [4 8] [8 4] [4 8] [8 4] [4 8] [8 4])

  ;; 9. Macarena — cycle through hip-out, head-touch, arms-up, hands-down.
  (dance-fragment macarena towards
    [3 9] [4 8] [12 12] [6 6] [3 9] [4 8] [12 12] [6 6])

  ;; 10. YMCA — the iconic Y, M, C, A shapes, twice through.
  (dance-fragment ymca towards
    [11 1] [7 5] [9 9] [12 12] [11 1] [7 5] [9 9] [12 12])

  (arm-diagrams->images "compositions/lilypond/resources" "resources"))

(write-ly! "10-dance-fragments.ly"
  (realize (music-lilypond-realizer #:title "Ten Dance Fragments")
           ten-dances))

(printf ";; done\n")
