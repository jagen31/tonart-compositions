#lang scribble/acmart
@require{references.rkt}
@require[scriblib/footnote]
@require[(except-in scribble/manual index)
         scribble-abbrevs/manual (for-label tonart)
         (except-in scribble/eval examples)
         scribble/example
         scribble/bnf
         (for-syntax racket/base syntax/parse)]

@title{Demo}
 
@(define-syntax chord->notes (make-rename-transformer #'chord->notes/simple))

@(define helper-eval (make-base-eval))
@interaction-eval[#:eval helper-eval
                  (require art/sequence/ravel tonart 2htdp/image)]

@(define-syntax (my-bnf stx)
   (syntax-parse stx
     [(_ bnf ...)
      #`(begin
      @para{}
        (let ([#,(datum->syntax stx 'open) @litchar{(}]
                [#,(datum->syntax stx 'close) @litchar{)}])
          @BNF[bnf ...]))]))

@section{Introduction}

We begin this demo by exploring Tonart's syntax.  Tonart is extensible at several points.  To
reflect this, we'll build our Tonart a few forms at a time.

@section{Tonart Syntax}

We begin with basic Tonart syntax.  The undefined nonterminals are extension points and will
progessively defined throughout the demo.
@linebreak[]
@(my-bnf
   (list @nonterm{form}
         @nonterm{art-id}
         @nonterm{object}
         @nonterm{rewriter}
         @nonterm{context}
         @BNF-seq-lines[
           (list @BNF-seq[open @code{@"@"} @BNF-seq[open @kleenestar[@nonterm{coord} close] close]
                 @hspace[1] @BNF-seq[@kleenestar[@nonterm{form}] close]])])

   (list @nonterm{program}
         @BNF-seq[open @code{define-art} @nonterm{art-id} @kleenestar[@nonterm{form}] close]
         @BNF-seq-lines[
           (list @BNF-seq[open @code{realize} @nonterm{realizer} @kleenestar[@nonterm{form}] close])]))
@linebreak[]
A context is a coordinate structure.  Tonart's primary coordinate structure is called @code{music}.
@linebreak[]
@(my-bnf 
  (list @nonterm{context}
        @BNF-seq[open @code{music} @kleenestar[@nonterm{<form>}] close]))
@linebreak[]
@code{music} has two coordinates,
@linebreak[]
@(my-bnf
   (list @nonterm{coordinate}
         @BNF-seq[open @code{interval} @BNF-seq[open @nonterm{number} @nonterm{number} close] close]
         @BNF-seq[open @code{voice} @kleenestar[@nonterm{id}] close]))
@linebreak[]
which are orthogonal and represent the horizontal (time) and vertical (voice)
dimensions of a physical score.

The @"@" form is used to embed objects into a context at given coordinates.

@section{Composing In Tonart}

Tonart is compiled into Racket by @emph{realizers}.  We will begin composing with only one object
and one realizer.
@linebreak[]
@(my-bnf 
  (list @nonterm{object}
        @BNF-seq[open @code{note} @nonterm{pitch} @nonterm{accidental} @nonterm{octave} close])
  (list @nonterm{realizer}
        @BNF-seq[open @code{staff-realizer} @nonterm{clef} close]))
@centered{------}
@(examples
  #:eval helper-eval
  #:label #f
  #:no-prompt
  (realize (staff-realizer [300 150] [treble])
    (|@| [(interval [0 4]) (voice soprano)]
      (note c 0 5))))
This is a note called @tt{C5}, or, C in the fifth octave.  It is sung by the soprano voice, for four 
beats.

To play this note from the computer, we will convert it into a frequency.  A frequency will
be represented by the @code{tone} object.  To turn the note into a tone, we will use the
@code{note->tone} rewriter.  Tonart rewriters are not put into the context; instead, they
add and remove objects from the context, in a context sensitive manner. 
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

@codeblock{
  (realize (sound-realizer)
    (@"@" [(interval [0 4]) (voice soprano)] 
      (note c 6))
    (note->tone))
}

Now we will add a harmony to this note.  We will express the harmony as a chord.
@linebreak[]
@(my-bnf 
   (list @nonterm{object}
         @litchar{...}
         @BNF-seq[open @code{chord} @nonterm{pitch} @nonterm{accidental} @nonterm{quality} close])
   (list @nonterm{rewriter}
         @litchar{...}
         @BNF-seq[open @code{chord->notes} @nonterm{octave} close]))
@centered{------}
@(examples
  #:eval helper-eval
  #:label #f
  #:no-prompt
  (realize (staff-realizer [300 150] [bass])
    (|@| [(interval [0 4]) (voice harmony)]
      (chord c 0 [M])
      (chord->notes/simple 3))))

We have not yet discussed putting objects one after another in time.  We could of course use
consecutive intervals.  However, this gets unwieldy.  We are instead going to establish a concept
of a @emph{sequence} of notes.
@linebreak[]
@(my-bnf 
   (list @nonterm{context}
         @litchar{...}
         @BNF-seq[open @code{seq} @kleenestar[@nonterm{number}] close])
   (list @nonterm{coordinate}
         @litchar{...}
         @BNF-seq[open @code{index} @kleenestar[@nonterm{number}] close]))
@linebreak[]
This defines a new context.  This context is called @code{seq} and has one coordinate, @code{index},
representing the position of an object in the context.  @code{seq} contexts can be embedded in music
contexts, allowing us to express an ordered sequence of notes directly on a score, without 
specifically giving lengths to the notes.
@linebreak[]
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

Note: Instead of writing,
@codeblock{
  (seq 
    (@"@" [(index 0)] (note a 3)) 
    (@"@" [(index 1)] (note b 3))
    ...)} 
I will write @code{(seq (notes [a 3] [b 3] ...))}.  

Below, a melody, in the soprano, is bound to @code{melody}.
The definition does not use @code{apply-rhythm}.  That is fine,
as we will apply the rhythm at the end.  It is often best
in Tonart to express the music by writing down all objects
and doing a series of rewrites at the end.
@(examples
  #:eval helper-eval
  #:label #f
  #:no-prompt
  (define-art melody
    (|@| [(voice soprano)]
      (seq (notes [c 0 5] [b -1 4] [a 0 4] 
                  [b 0 4] [c 0 5]))
      (rhythm 1.5 0.5 0.5 1.5 2))))
Now, we supply a harmony.
@(examples
  #:eval helper-eval
  #:label #f
  #:no-prompt
  (define-art harmony
    (|@| [(voice harmony)]
      (seq (chords [f 0 M] [c 0 M] [f 0 M] 
                   [g 0 M] [c 0 M]))
      (rhythm 1 1 1 1 2))))
A very nice cadence.  To see it visualized:
@(examples
  #:eval helper-eval
  #:label #f
  #:no-prompt
  (define-art song-notes 
    melody harmony 
    (apply-rhythm) (chord->notes/simple 3))
  (realize 
    (staff-realizer [300 280] [treble])
    song-notes))
To hear it:
@codeblock{
  (realize (sound-realizer) 
    song-notes (note->tone))
}

@section{Finale}

Now we will try compiling a piece with a more obscure object.
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

@code{function->notes} has an interval and a note range, and applies to
@code{function}s.  It requires a rhythm in the surrounding context of the
function and a continuously defined harmony for the extent of the function.
With those forms under those conditions, it produces a melody having the contour
of the function, within the given note range, having the proper rhythm, and
fitting within the harmony.

Here is an example.  I will use @code{(uniform-rhythm 1/4)} as a shorthand for 
@code{(rhythm 1/4 1/4 1/4 ...)}.
@(examples
  #:eval helper-eval
  #:label #f
  #:no-prompt
  #;(realize (staff-realizer)
    melody harmony
    (apply-rhythm)
    (|@| [(voice harmony) (interval [0 6])]
      (function (x) (sin x))
      (uniform-rhythm 0.25)
      (function->notes [(- pi) pi] [(a 3) (a 4)]))))
