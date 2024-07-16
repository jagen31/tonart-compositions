#lang scribble/acmart
@require{references.rkt}
@require[scriblib/footnote]
@require[(except-in scribble/manual index)
         scribble-math
         scribble-abbrevs/manual (for-label tonart)
         (except-in scribble/eval examples)
         scribble/example
         scribble/bnf
         (for-syntax racket/base syntax/parse)]

@define-footnote[racket-note make-racket-note]
@define-footnote[musescore-note make-musescore-note]

@title{Demo}
 
@(define helper-eval (make-base-eval))
@interaction-eval[#:eval helper-eval
                  (require art/sequence/ravel (except-in tonart staff-realizer) 2htdp/image "demo.rkt"
                           (for-syntax racket/math racket/base syntax/parse))

       ]

@(define-syntax (my-bnf stx)
   (syntax-parse stx
     [(_ bnf ...)
      #`(begin
      @para{}
        (let ([#,(datum->syntax stx 'open) @litchar{(}]
                [#,(datum->syntax stx 'close) @litchar{)}])
          @BNF[bnf ...]))]))

@section{Introduction}

This demo is intended to show how Tonart can be used for practical composition
with a score containing abstract music objects.  Notation softwares such as
MuseScore@musescore-note{https://musescore.org/en} offer a score interface and
many surface level transformations over notes and attributes of notes.  However,
they fail to provide operations over objects like chords, scale degrees, and
voice leadings.  Existing computer music libraries such as
Euterpea@~cite[euterpea-2014] provide these operations or the means to build
them, but do not provide them within a score-like interface.  A score-based DSL
is fundamental to leveraging existing knowledge and skills with score notation,
a centuries old medium.

@make-musescore-note[]

The demonstration will begin with only the base Tonart syntax.  We will
gradually add more syntaxes as we need them in our composition.  We will show
the score and play the audio output of our composition, at each step.

@section{Tonart Syntax}

Below is the base Tonart syntax.  The undefined nonterminals are extension points which will be
elaborated as we progress through the demo.
@linebreak[]
@(my-bnf
   (list @nonterm{form}
         @nonterm{art-id}
         @nonterm{object}
         @nonterm{rewriter}
         @nonterm{context}
         @BNF-seq-lines[
           (list @BNF-seq[open @code{@"@"} @BNF-seq[open @kleenestar[@nonterm{coordinate}] close]
                 @BNF-seq[@kleenestar[@nonterm{form}] close]])])

   (list @nonterm{program}
         @BNF-seq[open @code{define-art} @nonterm{art-id} @kleenestar[@nonterm{form}] close]
         @BNF-seq-lines[
           (list @BNF-seq[open @code{realize} @nonterm{realizer} @kleenestar[@nonterm{form}] close])]))
@linebreak[]
A @emph{context} is a coordinate structure.  Tonart's primary coordinate structure is called @code{music}.
@linebreak[]
@(my-bnf 
  (list @nonterm{context}
        @BNF-seq[open @code{music} @kleenestar[@nonterm{<form>}] close]))
@linebreak[]
Music has two coordinates,
@linebreak[]
@(my-bnf
   (list @nonterm{coordinate}
         @BNF-seq[open @code{interval} @BNF-seq[open @nonterm{number} @nonterm{number} close] close]
         @BNF-seq[open @code{voice} @kleenestar[@nonterm{id}] close]))
@linebreak[]
which are orthogonal and represent the horizontal (time) and vertical (voice)
dimensions of a physical score.  

The @code{@"@"} form is used to embed objects into a context at given coordinates.

Tonart is compiled into Racket@racket-note{Tonart is written as an embedded DSL in Racket.
It is a syntactic abstraction, running entirely at compile time, utilizing Racket's module system
to implement its own extension mechanisms and libraries.@~cite[ywiw-icfp-2002]} by @emph{realizers}.  

Note that we will not actually write out the @code{music} form in 
this demo, as the realizers we are using treat their toplevel forms as music implicitly.

@make-racket-note[]

@section{Composing In Tonart}

We will begin composing with only one object and one realizer.
@linebreak[]
@(my-bnf 
  (list @nonterm{object}
        @BNF-seq[open @code{note} @nonterm{pitch} @nonterm{accidental} @nonterm{octave} close])
  (list @nonterm{realizer}
        @BNF-seq[open @code{staff-realizer} close]))
@centered{------}
@(examples
  #:eval helper-eval
  #:label #f
  #:no-prompt
  (realize (staff-realizer)
    (|@| [(interval [0 4]) (voice soprano)]
      (note c 0 5))))
This is a note called @tt{C5}, or, C in the fifth octave.  It is sung by the soprano voice, for four 
beats.

To play this note from the computer, we will convert it into a frequency.  A frequency will
be represented by the @code{tone} object.  To turn notes into tones, we will use
a straightforward rewriter called @code{note->tone}. Tonart rewriters are not
composed into the context like objects; instead, they transform the context by
adding, deleting, and modifying existing objects.  Forms such as 
@code{define-art}, @code{realize}, and @code{@"@"} evaluate their subforms from
top to bottom, applying rewriters only to the objects denoted above them.
@linebreak[]
@(my-bnf 
   (list @nonterm{object}
         @litchar{...}
         @BNF-seq[open @code{tone} @nonterm{frequency} close])
   (list @nonterm{rewriter}
         @BNF-seq[open @code{note->tone} close])
   (list @nonterm{realizer}
         @litchar{...}
         @BNF-seq[open @code{sound-realizer} close]))
@centered{------}
@codeblock{
  (realize (sound-realizer)
    (@"@" [(interval [0 4]) (voice soprano)] 
      (note c 0 5))
    (note->tone))
}
Now we will add a harmony to this note.  We will express the harmony as a chord.
@linebreak[]
@(my-bnf 
   (list @nonterm{object}
         @litchar{...}
         @BNF-seq-lines[
            (list @BNF-seq[open @code{chord} @nonterm{pitch} @nonterm{accidental}])
            (list @hspace[1] @BNF-seq[open @nonterm{quality} close] close)])
   (list @nonterm{rewriter}
         @litchar{...}
         @BNF-seq[open @code{chord->notes} @nonterm{octave} close]))
@centered{------}
For this demo, @code{chord->notes} simply writes in the first three
possible notes for each chord within its bounds, creating so-called `snowman' triads.  The number
specifies which octave the chords will start in.
@(examples
  #:eval helper-eval
  #:label #f
  #:no-prompt
  (realize (staff-realizer)
    (|@| [(interval [0 4]) (voice accomp)]
      (chord c 0 [M])
      (chord->notes 3))))

We have not yet discussed putting objects one after another in time.  We could
of course use consecutive intervals.  However, this gets unwieldy.  We are
instead going to establish a concept of a @emph{sequence} of notes.
@linebreak[]
@(my-bnf 
   (list @nonterm{context}
         @litchar{...}
         @BNF-seq[open @code{seq} @kleenestar[@nonterm{form}] close])
   (list @nonterm{coordinate}
         @litchar{...}
         @BNF-seq[open @code{index} @kleenestar[@nonterm{number}] close]))
@linebreak[]
We define a new context.  This context is called @code{seq} and has one coordinate, @code{index},
representing the position of an object in the context.  @code{seq} contexts can be embedded in music
contexts, allowing us to express an ordered sequence directly in a score, without 
giving specific lengths to the notes or other objects it contains.

Next, we define syntax for rhythms, which are, for our purposes, a series of consecutive durations.  
@linebreak[]
@(my-bnf 
  (list @nonterm{object}
        @litchar{...}
        @BNF-seq[open @code{rhythm} @kleenestar[@nonterm{number}] close])
  (list @nonterm{rewriter}
        @litchar{...}
        @BNF-seq[open @code{apply-rhythm} close]))
@linebreak[]
Now we can do something more complex with the soprano.  
@linebreak[]
Note: Instead of writing,
@codeblock{
  (seq 
    (@"@" [(index 0)] (note a 0 3)) 
    (@"@" [(index 1)] (note b 0 3))
    ...)} 
I will write @code{(seq (notes [a 0 3] [b 0 3] ...))}.  
@linebreak[]
Below, a melody is bound.

@(examples
  #:eval helper-eval
  #:label #f
  #:no-prompt
  (define-art melody
    (seq (notes [c 0 5] [b -1 4] [a 0 4] 
                [b 0 4] [c 0 5]))
    (rhythm 3/2 1/2 1/2 3/2 2)))

Now, we supply a harmony, which the accompaniment will outline. 

@(examples
  #:eval helper-eval
  #:label #f
  #:no-prompt
  (define-art harmony
      (seq (chords [f 0 M] [c 0 M] [f 0 M] 
                   [g 0 M] [c 0 M]))
      (rhythm 1 1 1 1 2)))
We compose them together, and rewrite the piece into notes.
@(examples
  #:eval helper-eval
  #:label #f
  #:no-prompt
  (define-art song-notes 
    (voice@ (soprano) melody)
    (voice@ (accomp) harmony)
    (apply-rhythm) (chord->notes 3)))

Note that the use of @code{apply-rhythm} above applies both the rhythm of the
melody, and the harmonic rhythm of the harmony. 
@linebreak[]
To see it visualized:
@(examples
  #:eval helper-eval
  #:label #f
  #:no-prompt
  (realize (staff-realizer) 
    song-notes))
To hear it:
@codeblock{
  (realize (sound-realizer) 
    song-notes (note->tone))
}
@section{Finale}

To finish off, we will try adding a more obscure object to our composition.
@linebreak[]
@(my-bnf 
   (list @nonterm{object}
         @litchar{...}
         @BNF-seq[open @code{function} @BNF-seq[open @nonterm{id} close] @nonterm{expr} close])
   (list @nonterm{rewriter}
         @litchar{...}
         @BNF-seq-lines[
           (list @BNF-seq[open @code{function->notes}])
           (list @hspace[1] @BNF-seq[open @nonterm{number} @nonterm{number} close])
           (list @hspace[1] @BNF-seq[open @nonterm{note} @nonterm{note} close close])]))
@linebreak[]
@code{function} is a mathematical function.

@code{function->notes} applies to functions, and it creates a melody that fits within the surrounding 
harmony and matches the contour of the function.

Here is the finished work. I will use @code{(uniform-rhythm 1/4)} as a shorthand for 
@code{(rhythm 1/4 1/4 1/4 ...)}.  The function is @${sin(x)} over @${(-\pi,\pi)}.
@(examples
  #:eval helper-eval
  #:label #f
  #:no-prompt
  (realize (staff-realizer)
    song-notes
    (|@| [(voice countermelody)]
      harmony (apply-rhythm)
      (|@| [(interval [0 5])]
        (function (x) (sin x))
        (uniform-rhythm 1/4))
      (|@| [(interval [5 6])] (note e 0 4))
      (function->notes [(- pi) pi] [(a 3) (a 4)]))))
