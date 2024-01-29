#lang racket

(require (except-in tonart location) (only-in 2htdp/image scale) (prefix-in ta: (only-in tonart staff-realizer time-sig divisions music-rest load-musicxml)) (for-syntax syntax/parse racket/math racket/set))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-art-realizer staff-realizer
  (λ (stx)
    (syntax-parse stx
      [(_)
       #:with soprano (datum->syntax stx 'soprano)
       #:with countermelody (datum->syntax stx 'countermelody)
       #:with accomp (datum->syntax stx 'accomp)
       #:do [(define num-voices (length (set->list (apply set (map (λ (x) (syntax->datum (car (expr-voice x)))) (filter (compose not null? expr-voice) (current-ctxt)))))))]
       #`(scale 1/3 
           (realize (ta:staff-realizer [800 #,(* 200 num-voices)] 
                    {[soprano treble] [countermelody treble] [accomp bass]})
           #,@(lookup-ctxt)
           (delete time-sig)
           (delete divisions) 
           (delete music-rest)
           ))])))

(define-art-rewriter load-musicxml
  (λ (stx)
    (syntax-parse stx
      [(_ file) #'(context (ta:load-musicxml file [soprano alto]) (musicxml->tonart))])))
                  
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

(require (only-in "../room.rkt" location))
(define-art room-conf-1
  (i@ [0 4] (voice@ [soprano] (location main-stage)))
  (i@ [0 4] (voice@ [alto] (location balcony))))

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
