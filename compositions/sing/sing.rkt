#lang racket

(require tonart (for-syntax syntax/parse racket/system))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-art-object (lyric [word]))
(define-art-rewriter lyrics
  (λ (stx)
    (syntax-parse stx
      [(_ l ...)
       (qq-art stx (ix-- (lyric l) ...))])))

(define-mapping-rewriter (lyric->pitched-lyric [(: l lyric)])
  (λ (stx l)
    (define/syntax-parse (_ f) (require-context (lookup-ctxt) l #'tone))
    (syntax-parse l
      [(_ word)
       (qq-art l (pitched-lyric word f))])))

(define-art-object (pitched-lyric []))

(define-for-syntax BASE-PITCH 185)

(define-art-rewriter pitched-lyric->sound
  (λ (stx)
    (define-values (names syntaxes)
      (for/fold ([names '()] [syntaxes '()] #:result (values (reverse names) (reverse syntaxes)))
                ([pl (current-ctxt)])
        (syntax-parse pl
          [(_ word pitch)
           (define word* (syntax-e #'word))
           (define pitch* (syntax-e #'pitch))
           (define name (gensym 'lyric_))
           (system (format "say -v zarvox -r 180 '~a' -o ~a" word* name))
           (system (format "sox '~a.aiff' '~a.wav' pitch ~a" name name (* 1200 (log (/ pitch* BASE-PITCH) 2))))
           (values (cons name names) (cons (qq-art pl (sound #,name)) syntaxes))]
          [_ (values names syntaxes)])))
    #`(context 
       #,(qq-art stx
           (sound-map #,@(for/list ([n names]) #`[#,n . #,(format "~a.wav" n)])))
        #,@(map delete-expr syntaxes)
        #,@syntaxes)))

(define-art happy-birthday-lyrics
  (lyrics 
    "Hap" "py" "birth" "day" "to" "you"
    "Hap" "py" "birth" "day" "to" "you"
    "Hap" "py" "birth" "day" "Dear" "Je" "han"
    "Hap" "py" "birth" "day" "to" "you"))