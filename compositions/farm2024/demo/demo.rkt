#lang racket

(require tonart (except-in 2htdp/image bitmap) art/sequence/ravel racket/math (prefix-in ta: tonart) 
         (prefix-in rack: (only-in racket #%top-interaction)) syntax/parse/define
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

;; a staff-realizer which is set up for our demo voices
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

;; realizer which sends and advances time
(define-art-realizer* play-realizer
  (λ (stx)
    #`(begin
        (chuck (send demo-prelude #,@(current-ctxt) (demo-postlude)))
        (chuck (advance-time 4)))))

;; shorthand for play realizer
(define-syntax !
  (syntax-parser
    [(_ expr ...)
     #'(realize (play-realizer) expr ...)]))

;; shorthand for staff realizer
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

(define-art opening
  (voice@ [accomp] (trumpet-blast [c 0 4] [f 0 4])) (note->midi))
(define-art opening2
  (-- [8]
      [4 (voice@ (soprano) (trumpet-blast [g 0 4] [a 0 4]))
          (voice@ (bass) (trumpet-blast [c 0 2] [f 0 2]))]
      [4 (voice@ (soprano) (trumpet-blast [c 0 5] [f 0 5]))
         (voice@ (accomp) (trumpet-blast [c 0 4] [f 0 4]))
         (voice@ (bass) (trumpet-blast [c 0 2] [f 0 2]))])
  (voice@ (accomp) (constant-structure) (structure-notes [M])
          (chord->notes/simple 3))
  (note->midi))

(define-art the-script
  (ix--
   (speak "Welcome to my Tonart demo!")
   (output (! opening))
   (pause 4)
   
   (speak "Tonart is a language for composing music.")
   (speak "The major idea behind the language is composition in a score like context with extensible music notation.")
   (speak "You can think of it as abstract syntax for music scores...")
   (speak "Or, more operationally, you can think of it like a stack-based language...")
   (speak "...except instead of pushing values onto a stack, you are writing music syntax onto a music score.")
   (speak "and instead of running operators over parts of the stack, you run rewrites over parts of the score.")
   (speak "so, we specify a score structure and write notations within it.")
   (speak "the standard way the score structure is specified is by using two 'coordinates', voice and interval.")
   (speak "These are roughly the vertical and horizontal dimensions of staff paper.")
   
   (output #<<<
(@ [(voice soprano) (interval [0 4])]
  .....)
<
           )
   (speak "To start writing music in tonart, we use the @-form")
   (speak "The @-form indicates that any notations within the body will be located at the provided coordinates.")
   (speak "This indicates that whatever we write will be in the soprano voice, from beat 0 to 4.")
   (pause 4)
   
   (output #<<<
(@ [(voice soprano) (interval [0 4])]
  (note c 0 5))
<
              )
   (speak "we will start with our first notation, musical note")
   (speak "this is the iconic note you think of when you think of sheet music.")
   (speak "here is a note for the soprano!  this is C in the 5th octave")
   (pause 2)
   (speak "however, we can't just evaluate this.  This is a tonart form, in a racket repl.")
   (speak "we need a process for going from tonart to racket.")
   (speak "I call this process 'realization'. The reason I call it this is because its")
   (speak "like, taking this notation, this piece of art, and turning it into a reality")
   (speak "well, in the case of this embedded DSL, turning it into Racket.  Racket reality.")
   
   
   
   (speak "our first realizer is the staff realizer.")
   (speak "using the staff realizer, we can see a score paper representation")
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
   (speak "oh.... that didn't work.  Something to realize about realizers is- they only understand a certain input language.")
   (speak "just like handing a human performer a list of frequencies and durations isn't going to do much good,")
   (speak "handing the play-realizer sheet music is simply not going to cut it.")
   (speak "We can think of what we have as 'note music'")
   (speak "to get the computer to play it, we'll have to compile the note music into some other music language.")
   (speak "The play realizer recognizes two types of notations.")
   (speak "So we have a couple options")

   (speak "we'll use the 'note->tone' rewriter to compile to tones.")
   
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
   (speak "it sounds like this.")
   (pause 2)
   (speak "we'll use the 'note->midi' rewriter to compile to midi.")
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

   (speak "so, now we want to compose something.  lets give our soprano something more interesting.")
   (speak "I could write out each note with interval 0 to 2, C, interval 2 to 4, E... etc.")
   (speak "but how I really want to do it is: just specify a sequence of notes and a rhythm,")
   (speak "and rewrite that into note music.")
   (speak "we'll use a special form for it.  It will be a _context_.")
   (speak "Tonart uses a context called 'music' with coordinates voice and interval.")
   (speak "this will be a context which has one coordinate, `index`, and we will call it `seq` (short for sequence)")
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

   (speak "of course i'm sure you want to know if it sounds good with the melody.  I won't keep you waiting.")

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
     (function->contour (- pi) pi)))
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

   (speak "let's play it of course...")

      (output #<<<
(! (voice@ (descant) melody2) (note->midi))
<
              )
   (pause 2)
   (output
    (! (voice@ (descant) melody2) (note->midi)))
   (pause 12)
   
   (speak "That's everything I want for the composition!")
   (speak "Lets check out the whole thing together!")

   (pause 2)

   (output #<<<
(= (voice@ [soprano] melody)
   (voice@ [descant] melody2 (i@ [10 12] (note c 0 4)))
   (voice@ [accomp] harmony)
   
   (chord->notes/simple 3))
<
           )

   (speak "[ I handwrote a final note for our second melody ]")

   (pause 2)

   (execute
    (= (voice@ [soprano] melody)
       (voice@ [descant] melody2 (i@ [10 12] (note c 0 4)))
       (voice@ [accomp] harmony)
       
       (chord->notes/simple 3)))

   (pause 8)

   (speak "and finally we'll play it...")

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

   (pause 16)

   (speak "I hope you enjoyed!")

   (pause 4)

   (speak "before we go, I want to show you a couple things.")
   (speak "first, here is the opening to a real piece I'm working on, which uses the same techniques we used today.")


      (output #<<<
(execute (require "../../random/test.rkt"))
<
           )
   (execute (require "../../random/test.rkt"))

   (output (= default-notes))
   (pause 4)
   (execute (! default-notes (note->midi)))

   (pause 40)

   (speak "finally, this script I am reading off of is also written in art...")
   (speak "and while I think I've done a good job performing it.  There's also a computer performance!")
   (speak "so let me introduce my co presenter...")
   
   (execute (begin (! opening2) (realize (perform-script) the-script)))))

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

(provide (all-defined-out))

#;(define-syntax (#%top-interaction stx)
  (println stx)
  (syntax-parse stx
    [(_ {~datum ->} n:number) #'(chuck (advance-time n))]
    [(_ {~datum !} expr ...) 
     (print #'(chuck (send demo-prelude expr ... (demo-postlude))))
     #'(chuck (send demo-prelude expr ... (demo-postlude)))]
    [(_ . expr)
     #'(rack:#%top-interaction . expr)]))