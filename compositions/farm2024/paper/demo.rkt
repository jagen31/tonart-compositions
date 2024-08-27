#lang racket

(require tonart (prefix-in ta: (only-in tonart staff-realizer)) (for-syntax syntax/parse racket/math racket/set))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-art-realizer staff-realizer
  (λ (stx)
    (syntax-parse stx
      [(_)
       #:with soprano (datum->syntax stx 'soprano)
       #:with countermelody (datum->syntax stx 'countermelody)
       #:with accomp (datum->syntax stx 'accomp)
       #:do [(define num-voices (length (set->list (apply set (map (λ (x) (syntax->datum (car (expr-voice x)))) (filter (compose not null? expr-voice) (current-ctxt)))))))]
       #`(realize (ta:staff-realizer [290 #,(* 110 num-voices)] 
                    {[soprano treble] [countermelody treble] [accomp bass]})
           #,@(current-ctxt))])))
                  
(define-art-rewriter chord->notes
  (λ (stx)
    (syntax-parse stx
      [(_ n) #'(chord->notes/simple n)])))


(define-art-rewriter function->notes
   (λ (stx)
     (syntax-parse stx
       [(_ [l h] [nl hl])
        (qq-art stx
          (context
            (function->image l h)
            (image->point-set)
            (rhythm->holes)
            (note-range nl hl)
            (chord->scalar-note-seq)
            (fill-holes-from-points)
            (delete point-set)
            (seq-ref)))])))

#|
\documentclass[10pt, sigplan, screen]{acmart}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}

\bibliographystyle{ACM-Reference-Format}

%%% The following is specific to FARM '24 and the paper
%%% 'Demo: Composable Compositions with Tonart'
%%% by Jared Gentner.
%%%
\setcopyright{rightsretained}
\acmDOI{10.1145/3677996.3678294}
\acmYear{2024}
\copyrightyear{2024}
\acmISBN{979-8-4007-1099-5/24/09}
\acmConference[FARM '24]{Proceedings of the 12th ACM SIGPLAN International Workshop on Functional Art, Music, Modelling, and Design}{September 2, 2024}{Milan, Italy}
\acmBooktitle{Proceedings of the 12th ACM SIGPLAN International Workshop on Functional Art, Music, Modelling, and Design (FARM '24), September 2, 2024, Milan, Italy}
\acmSubmissionID{icfpws24farmmain-p70-p}
\received{2024-06-02}
\received[accepted]{2024-07-02}

|#