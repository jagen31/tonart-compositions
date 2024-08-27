#lang racket

(require tonart art/sequence/ravel racket/math (prefix-in ta: tonart) (prefix-in rack: (only-in racket #%top-interaction)) syntax/parse/define
         (for-syntax syntax/parse racket/set racket/math))

(start-chucker chuck)

(define-mapping-rewriter (transpose-octave [(: n note)])
  (λ (stx n)
    (syntax-parse stx
      [(_ n*:number)
       (syntax-parse n
         [(_ p a o) (qq-art n (note p a #,(+ (syntax-e #'n*) (syntax-e #'o))))])])))

(define-art-rewriter function->contour
  (λ (stx)
    (syntax-parse stx
      [(_ w h)
       (qq-art stx (function->image w h))])))
  

(define-art demo-prelude
  (voice@ (soprano) (channel 1)) 
  (voice@ (accomp) (channel 2)) 
  (voice@ (descant) (channel 1)) 
  (voice@ (bass) (channel 3))
  (tuning 12tet)
  (volume 1))

(define-art-rewriter demo-postlude
  (lambda (stx)
    #'(context 
        (midi->full-midi) (tone->full-tone) (d/dt))))

(define-art-rewriter contour->notes 
  (λ (stx)
    (qq-art stx 
      (context
        (image->point-set)
        (chord->scalar-note-seq)
        (rhythm->holes)
        (fill-holes-from-points)
        (seq-ref)
        (delete point-set)))))

(define-art-realizer staff-realizer
  (λ (stx)
    (syntax-parse stx
      [(_ [w h])
       #:with soprano (datum->syntax stx 'soprano)
       #:with countermelody (datum->syntax stx 'countermelody)
       #:with accomp (datum->syntax stx 'accomp)
       #:do [(define num-voices (length (set->list (apply set (map (λ (x) (syntax->datum (car (expr-voice x)))) (filter (compose not null? expr-voice) (current-ctxt)))))))]
       #`(realize (ta:staff-realizer [w h] 
                    {[soprano treble] [descant treble] [accomp bass]
                     [one bass] [two treble] [three bass]})
           #,@(current-ctxt))])))

(define-syntax ->
  (syntax-parser
    [(_ n:number) #'(chuck (advance-time n))]))

(define-art-realizer* play-realizer
  (λ (stx)
    #`(begin
        (chuck (send demo-prelude #,@(current-ctxt) (demo-postlude)))
        (chuck (advance-time 4)))))

(define-syntax !
  (syntax-parser
    [(_ expr ...)
     #'(realize (play-realizer) expr ...)]))

(define-syntax =
  (syntax-parser
    [(_ [w:number h:number] expr ...)
     #'(realize (staff-realizer [w h]) expr ...)]
    [(_ expr ...)
     #'(= [1800 600] expr ...)]))


(define-art-object (speak [text]))
(define-art-object (show-text [music]))
(define-art-object (output [music]))
(define-art-object (execute [music]))
(define-art-object (pause [t]))

(define-art-rewriter trumpet-blast
  (syntax-parser
    [(_ [note1 ...] [note2 ...])
     (qq-art this-syntax
             (context
              (seq (ix-- (note note1 ...) (note note1 ...) (note note1 ...) (note note2 ...)))
              (rhythm 1/4 1/4 1/4 2)
              (apply-rhythm)))]))

(define-art the-script
  (ix--
   (speak "Welcome to my Tonart demo!")
   (output (! (voice@ [accomp] (trumpet-blast [c 0 4] [f 0 4])) (note->midi)))
   (pause 4)
   (speak "Tonart is a language for composing music.")
   (speak "The major idea behind the language is to provide a score-like context for an extensible language of music notations.")
   (speak "You can think of it like a stack-based language...")
   (speak "...except instead of pushing values onto a stack, you are writing music syntax onto a music score.")
   (speak "the standard way the score structure is defined is by using two 'coordinates', voice and interval.")
   (speak "These are roughly the vertical and horizontal dimensions of staff paper.")
   
   (output #<<<
(@ [(voice soprano) (interval [0 4])]
  .....)
<
           )
   (speak "The @-syntax indicates that any notations within the body will be located at the provided coordinates.")
   (speak "This indicates that whatever we write will be in the soprano voice, from beat 0 to 4.")
   (pause 4)
   
   (output #<<<
(@ [(voice soprano) (interval [0 4])]
  (note c 0 5))
<
              )
   (speak "and here is a note for the soprano!") 
   (pause 4)
   (speak "using the staff realizer, we can see it on score paper")
   (output #<<<
(realize (staff-realizer [800 200])
  (@ [(voice soprano) (interval 0 4)]
    (note c 0 5)))
<
              )
   (pause 2)
   (output
    (realize (staff-realizer [800 200])
             (@ [(voice soprano) (interval 0 4)]
                (note c 0 5))))

   (pause 4)

   (speak "using the play realizer, we can get the computer to try to perform it")
   (output #<<<
(realize (play-realizer)
  (@ [(voice soprano) (interval 0 4)]
    (note c 0 5)))
<
              )

   (pause 4)
   (speak "oh.... that didn't work.  Something to realize about realizers is- they only understand a certain language.")
   (speak "just like handing a human performer a list of frequencies and durations isn't going to do much good,")
   (speak "handing the computer sheet music is simply not going to cut it.")

   (speak "The play realizer recognizes two types of notations- tones, and midi.")
   (speak "tones are, like.  The classic synthesized sound you get by programming a frequency into an oscillator.")
   (speak "MIDI, the musical instrument digital interface, is a very old standard for transmitting music events.")
   (speak "I have a digital pipe organ listening for midi events, so that's how we'll hear them rendered.")

   (speak "To produce a score the computer can read, we'll use Tonart's rewriters.")
   (speak "Rewriters are the workhorse of tonart.  They help a composer to compile one music language into another.")
   (speak "we have two straightforward rewriters here, 'note->tone' and 'note->midi'.")
   
   (output #<<<
(realize (play-realizer)
  (@ [(voice soprano) (interval 0 4)]
    (note c 0 5))
  (note->tone))
<
           )
   (pause 2)

   
   (output
    (! (@ [(voice soprano) (interval [0 4])]
          (note c 0 5))
       (note->tone)))
   (pause 4)
   (speak "that was the tone")
      (output #<<<
(realize (play-realizer)
  (@ [(voice soprano) (interval 0 4)]
    (note c 0 5))
  (note->midi))
<
           )
   (pause 2)
   
   (output
    (! (@ [(voice soprano) (interval [0 4])]
          (note c 0 5))
       (note->midi)))
   (pause 4)
   (speak "that was the midi")
   
   (speak "for convenience, i will abbreviate the staff realizer as \"=\" and the play realizer as \"!\", for the rest of the demo.")
   (speak "also, I will use 'voice@' and 'i@', for voice coordinate and interval coordinate respectively.")
   
   (output #<<<
(= (voice@ [soprano] (i@ [0 4] (note c 0 5))))
<
              )
   (pause 2)
   (output
    (= (voice@ [soprano] (i@ [0 4] (note c 0 5)))))
   (pause 4)
   (output #<<<
    (! (voice@ [soprano] (i@ [0 4] (note c 0 5))) (note->midi))
<
)
   (pause 2)
   (output (! (voice@ [soprano] (i@ [0 4] (note c 0 5))) (note->midi)))
   (pause 4)

   (speak "so, we want to compose something.  lets give our soprano something more interesting.")
   (speak "we'll use a special form called 'seq'.  This is not an object or a rewriter.")
   (speak "This is a _context_.  Tonart uses a context called 'music' with coordinates voice and interval.")
   (speak "sequences use a context called seq, with one coordinate called 'index'.")
   (speak "rhythm is an object, apply-rhythm is a rewriter that takes a rhythm")
   (speak "with a seq in context, and puts the elements of the sequence one after the other in time,")
   (speak "matching the rhythm.")

   (pause 16)
   
   (output #<<<
    
(define-art melody
  (seq (notes [c 0 4] [d 0 4] [e -1 4] [e 0 4] [f 0 4] [d 0 4] [e -1 4]))
  (rhythm 2 2 2 2 1 1 2)
  (apply-rhythm))
<
              )
   (execute
    (define-art melody
      (seq (notes [c 0 4] [d 0 4] [e -1 4] [e 0 4] [f 0 4] [d 0 4] [e -1 4]))
      (rhythm 2 3 1 2 1 1 2)
      (apply-rhythm)))

   
   (speak "Here is what it looks and sounds like")
   (output #<<<
(= (voice@ [soprano] melody))
<
              )
   (pause 2)
   (output (= (voice@ [soprano] melody)))

   (pause 4)

   (output #<<<
(= (voice@ [soprano] melody))
<
           )
   
   (pause 2)
   
   (execute (! (voice@ [soprano] melody) (note->midi)))
   (pause 12)

   (speak "Now we'd like to make a harmony.  We'll do it in chords.")
   (speak "they tend to be a reductive expression of harmony...")
   (speak "...but they get the job done.  And they are the language of many performers- plenty of guitarists read chords, not notes.")
   (pause 16)
   (output #<<<
(define-art harmony
  (seq (chords [c 0 m] [d 0 m] [c 0 m] [c 0 M] [f 0 m] [g 0 m] [c 0 m]))
  (rhythm 2 2 2 2 1 1 2))
<
)
   (execute
    (define-art harmony
      (seq (chords [c 0 m] [d 0 m] [c 0 m] [c 0 M] [f 0 m] [g 0 m] [c 0 m]))
      (rhythm 2 2 2 2 1 1 2)
      (apply-rhythm)))
   
   (output #<<<
(= (voice@ (accomp) harmony) (chord->notes/simple 3))
<
              )
   (pause 2)
   (output
    (= (voice@ (accomp) harmony) (chord->notes/simple 3)))
   (pause 4)

   (output #<<<
(! (voice@ [accomp] harmony) (chord->notes/simple 3)
  (note->midi))
<
              )
   (execute
    (! (voice@ (accomp) harmony) (chord->notes/simple 3)
       (note->midi)))

   (pause 12)

   (speak "of course now you want to know if it sounds good with the melody.  I won't keep you waiting.")

   (output #<<<
(! (voice@ [soprano] melody)
   (voice@ [accomp] harmony)
   (chord->notes/simple 3)
   (note->midi))
<
              )

   (pause 2)

   (execute
    (! (voice@ [soprano] melody)
       (voice@ [accomp] harmony)
       (chord->notes/simple 3)
       (note->midi)))

   (pause 12)

   (speak "hopefully that sounded good to you!")
   (speak "now we will do something unconventional.  We will make a another melody.")
   (speak "THAT is not unconventional.  What's unconventional is we'll make it out of a function!")

   (speak "we don't have a realizer that can show a function yet.")
   (speak "So I'll just provisionally introduce the 'draw-music-realizer', abbreviated 'dmr'...")
   (speak "which does a worse job aesthetically than staff-realizer, but draws more things.")

   (output #<<<
(dmr (function (x) (sin x)))
<
           )

   (pause 2)

   (output
    (dmr (function (x) (sin x))))

   (pause 4)

   (speak "To make this work with our realizers, we need to turn it into notes or tones or something")
   (speak "we'll do it in a few steps.")

   (output #<<<
(dmr (i@ [0 8] (function (x) (sin x))
     (function->contour (- pi) pi)))
<
           )
   (speak "function->contour takes an x-range and produces a curve that looks like the graph of the function.")
   (pause 2)
   (output
    (dmr (i@ [0 8] (function (x) (sin x)))
         (function->contour (- pi) pi)))
   (pause 4)

   (speak "ok, we probably want something a little _wavier_")

   (output #<<<
(dmr (i@ [0 8] (function (x) (sin (* 4 x)))
     (function->contour)))
<
           )


   
   (pause 2)
   (output
    (dmr (i@ [0 8] (function (x) (sin (* 4 x))))
         (function->contour (- pi) pi)))
   (speak "that's better.")
   (pause 2)

   (speak "finally we have to supply a note range that the melody should have,")
   (speak "a rhythm for the melody, and a harmony the melody will fall within.")
   (speak "then we can rewrite with contour->notes, and have our melody.")

   (output #<<<
(define-art melody2
   (i@ [0 10]
     (function (x) (sin (* 4 x)))
     (function->contour (- pi) pi))
   (note-range [e -1 3] [e 0 4])
   (i@ [0 10] (uniform-rhythm 1/4))
   harmony)
   (contour->notes))
<
           )

   (execute
    (define-art melody2
      (i@ [0 10]
          (function (x) (sin (* 4 x)))
          (function->contour (- pi) pi))
      (note-range [e -1 3] [e 0 4])
      (i@ [0 10] (uniform-rhythm 1/4))
      harmony
      (contour->notes)))

   (pause 2)

   (speak "and here it is!")

   (output #<<<
(= (voice@ (descant) melody2))
<
              )
   (pause 2)
   (output
    (= (voice@ (descant) melody2)))
   (pause 4)
   
   (speak "That's everything I want for the composition!")
   (speak "lets check it out!")

   (pause 2)

   (output #<<<
(! (voice@ [soprano] melody)
   (voice@ [descant] melody2 (i@ [10 12] (note c 0 4)))
   (voice@ [accomp] harmony)
   
   (chord->notes/simple 3)
   (note->midi))
<
           )

   (pause 2)

   (execute
    (! (voice@ [soprano] melody)
       (voice@ [descant] melody2 (i@ [10 12] (note c 0 4)))
       (voice@ [accomp] harmony)
       
       (chord->notes/simple 3)
       (note->midi)))

   (pause 12)

   
   
   (execute
    (! (-- [8]
           [12 (voice@ (soprano) (trumpet-blast [g 0 4] [a 0 4]))
               (voice@ (bass) (trumpet-blast [c 0 2] [f 0 2]))])
       (note->midi)))

   (execute (realize (perform-script) the-script))))

(define-art-realizer perform-script
  (λ (stx)
    #`(begin #,@(for/list ([expr (sort (current-ctxt) < #:key expr-single-index)])
                  (syntax-parse expr
                    [({~literal speak} text)
                     #'(system (format "say -v Trinoids ~s" text))]
                    [({~literal show-text} text)
                     #`(displayln text)]
                    [({~literal execute} music)
                     #'music]
                    [({~literal output} music ...)
                     #'(begin (displayln music) ...)]
                    [({~literal pause} t:number) #'(sleep t)])))))
        

#;(define-syntax (#%top-interaction stx)
  (println stx)
  (syntax-parse stx
    [(_ {~datum ->} n:number) #'(chuck (advance-time n))]
    [(_ {~datum !} expr ...) 
     (print #'(chuck (send demo-prelude expr ... (demo-postlude))))
     #'(chuck (send demo-prelude expr ... (demo-postlude)))]
    [(_ . expr)
     #'(rack:#%top-interaction . expr)]))