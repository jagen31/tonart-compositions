#lang scribble/acmart
@require{references.rkt}

@require[(except-in scribble/manual index)
         scribble-abbrevs/manual (for-label tonart)
         (except-in scribble/eval examples)
         scribble/example]

@(define helper-eval (make-base-eval))
@interaction-eval[#:eval helper-eval
                  (require tonart 2htdp/image)]

@title{Demo}

In this demo, we will compose a piece of music borrowing ideas from several
different musical and non-musical traditions.  Some of these objects are
unfamiliar to most musicians, so the reader is not expected to be familiar
with them, either.  However, I will be explaining them in terms of more
well-known music objects, and ultimately compiling the whole score into these
well-known objects.  The objects are described below for context.

@section{Common Objects}

@subsection{Tones}

Generally speaking, a tone is a sound produced by a regular vibration at a specific frequency.
When a sound repeats at a fast enough rate it is perceived by the ear as a tone.  The frequency
corresponds to the "height" of the tone.  Higher frequency tones are perceived as being "higher".

@codeblock{
<freq>Hz ; Standard Syntax
440Hz ; Example

(tone <freq>) ; Tonart Syntax
(tone 440)  ; Example
}

A special property of how we perceive tones is: two tones that are double each
other's frequency sound "almost the same" (are hard to tell apart).  This is why
we can all sing the same songs together, even though we have different vocal
ranges.

@subsection{Notes}

For our purposes, notes are labels laid over top of tones, so that each note label
corresponds to a tone of a certain frequency.  

@codeblock{
;; Standard Syntax
<pitch-label><accidental><octave>
Ab5 ; Example

;; Tonart Syntax
(note <pitch-label> <accidental-number> <octave-number>)
(note a -1 5) ; Example
}

Notes are given labels @tt{A1 B1 C1 D1 E1 F1 G1 A2 B2 C2 D2 ...},
which, in that order, must have increasing corresponding frequencies.  C2 must
have a frequency exactly double C1.  We call the distance between two notes an
"interval" and this particular same-sounding, doubled-frequency interval an
"octave".  Two notes an octave apart will always have the same _pitch-label_,
just as in the above example.  The octave number is what will vary (C1, C2, C3,
C4 etc.).  An expanded set of tones can be expressed as a raising or lowering of
a note, which is done via an "accidental".  To raise a note, the symbol '#'
(pronounced "sharp") is used.  To lower a note, the symbol 'b' (pronounced
'flat') is used. The accidental is also preserved between octaves, and the
pitch-label taken with the accidental is called the "pitch class".  For example, a
raised C is written C#, each distinct pitch classes.  C# in the 4th octave is
written C#4.  The C# an octave above C#4 is written C#5.  For clairty- C#4 and
C#5 are in the same pitch class and that pitch class is C#.

@subsection{Tuning}

A tuning is a mapping of notes to tones.  In many cases, particularly keyboard
tuning, this means mapping one octave of symbols- @tt{C #/b D #/b E F #/b G #/b
A #/b B}- to reasonable frequencies, and finding all the others by doubling or
halving those frequencies (transposing by octaves).

@codeblock{
<tuning-name> ; Standard Syntax
12tet ; Example

(tuning <tuning-name>) ; Tonart Syntax
(tuning 12tet) ; Example
}

Here is an artwork demonstrating notes being mapped to tones in two different tunings:

First we draw the notes we want to tune.

@(examples
    #:eval helper-eval
    #:label #f
    #:no-prompt

    (define-art my-notes 
      (seq (notes [c 0 4] [d 0 4] [e 0 4])))
    (realize (draw-realizer [200 40]) my-notes))

Next, we place the notes in two different @code{music} contexts with the two tunings we want to
try. @code{namespace} is another type of context which is useful for certain things, though
just being used for show here.  @code{@"name@"} is a shorthand for ascribing a @code{name} coordinate.

@(examples
    #:eval helper-eval
    #:label #f
    #:no-prompt

    (define-art my-tunings
      (namespace 
        (name@ 12-tone-ET
          (music (tuning 12tet) my-notes))
        (name@ Just-Intonation
          (music (tuning 5-limit) my-notes))))
    (scale 1/2 (realize (draw-realizer [600 100]) my-tunings)))

Finally, we run @code{note->tone}.  Because of the way we wrote this art,
the code is slightly obfuscated with a few layers of @tt{rewrite-in} destructuring.
@code{(delete tuning)} is done to declutter the resulting image.

@(examples
    #:eval helper-eval
    #:label #f
    #:no-prompt
    (scale 1/2
      (realize (draw-realizer [600 100])
        my-tunings
        (rewrite-in-namespace
          (rewrite-in-music 
            (rewrite-in-seq (note->tone))
            (delete tuning))))))

One interesting thing to note is how the Just Intoned result is represented by
fractions, while the Equal Tempered result is represented by decimals.  This is 
indirectly a result of the fact that Just Intonation is tuned with
multiplication by whole number ratios, while Equal Temperament is tuned using
multiplication by an irrational number.

@subsection{Chords}

Chords are an abstraction over combinations of notes, which often function like 
a set of pitch classes.  For the purposes of this demo, our chord symbols will
specify sets of 3 pitch classes, called triads.  

@codeblock{
;; Standard Syntax
<pitch-label><accidental><quality-abbrev> 
or 
<pitch-class> <quality>
;; Example
F#m or F# minor

;; Tonart Syntax
(chord <pitch-label> <accidental-number> 
       [<quality>])
;; Example
(chord f 1 [m])
}

Any number of notes can be in a chord, including repeats and octave
transpositions.  Each note must be a member of one of the pitch classes
corresponding to the chord.  Not all pitch classes from the set need be present
for a chord to retain its identity.  The pitch label of the chord is called
the "root" of the chord, and must be present.  The "quality" of the chord
deals with the intervals between notes in the chord.  C major, CM, or simply C,
has pitch classes C, E, and G.  C minor or Cm has pitch classes C, Eb, and G.
These are the two chord qualities that will be used in the demo.

Here is an example of chords being rewritten to notes.  

@(examples
    #:eval helper-eval
    #:label #f
    #:no-prompt

    (define-art my-chords
      (seq (chords [c 0 M] [f 0 m] [g 1 M])))
    (realize (draw-realizer [200 40]) my-chords))

Next we turn them into notes.  The particular choice of notes is, as mentioned,
arbitrary.  The @code{chord->notes/simple} rewriter happens to stack the first
three notes in the chord, ascenting from the given octave.

@(examples
    #:eval helper-eval
    #:label #f
    #:no-prompt

    (realize (draw-realizer [300 50])
      (music 
        my-chords
        (rewrite-in-seq (chord->notes/simple 4)))))

@subsection{Keys and Degrees}

A key in music is similar to a chord, being defined by a root pitch and a modality.

@codeblock{
;; Standard Syntax
<pitch-class> <quality>
;; Example
F# minor

;; Tonart Syntax
(key <pitch-label> <accidental-number> <quality>)
;; Example
(key f 1 m)
}

The difference between the two is a key contains a number of different chords.  Keys are a
broader description of a harmony, containing a large number of notes, intervals, chords, and
even idioms which interact

A degree or scale degree represents a pitch class, made relative to the root of a key.

@codeblock{
^<number> ; Standard Syntax
^2 ; Example

(^ <number>) ; Tonart Syntax
(^ 2) ; Example
}

We will show the first four degrees of three different keys: C major, B-flat
minor, and G-sharp major.

@(examples
    #:eval helper-eval
    #:label #f
    #:no-prompt

  (define-art my-degrees
    (i@ [0 12] 
      (voice@ (alto)
        (seq (^s 1 2 3 4 1 2 3 4 1 2 3 4)) 
        (urhy* 1) (delete seq)))
    (-- [4 (key c 0 M)] [4 (key b -1 m)] 
        [4 (key g 1 M)]))
  
  (scale 1/2 
    (realize (draw-realizer [600 80]) 
      (music my-degrees))))

@(examples
   #:eval helper-eval
   #:label #f
   #:no-prompt

   (scale 1/2
     (realize (draw-realizer [600 80])
       (music my-degrees (octave 4) 
              (^->note) (delete octave)))))

@subsection{Rhythm}

Rhythm is the timing and accentuation of specific beats.  The
Tonart rhythm object is a useful tool, but is very basic and only 
captures the former.

@codeblock{
  (rhythm <number> ...) ; Tonart syntax
  (rhythm 1 1 1/2 1/2 1 1) ; Example
}

The series of numbers in the rhythm object denotes lengths
of time in beats.  A rewriter, @code{apply-rhythm}, can be
used to place a sequence in time.

@(examples
   #:eval helper-eval
   #:label #f
   #:no-prompt
  (define-art my-music 
    (seq (notes [f 1 3] [b 0 3] [e 0 3] [c 1 3] 
                [d 1 3] [e 0 3] [f 1 3]))
    (voice@ (bass) 
      (rhythm 2 1/2 1/2 1 1/4 1/4 3)))

  (scale 1/2 
    (realize (draw-realizer [600 100]) 
      (music my-music)))
  (scale 1/2 
    (realize (draw-realizer [600 100]) 
      (music my-music (apply-rhythm)))))
