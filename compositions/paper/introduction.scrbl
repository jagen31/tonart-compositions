#lang scribble/acmart
@require{references.rkt}
@require[scriblib/footnote]
@require[scribble/manual]

@title{Context}

In this demo, we will compose a piece of music borrowing language from several different
musical and non-musical traditions.  Some of the concepts are unknown to 
musicians, so it's not an expectation that the viewer is familiar with them,
either.  However, we will compile to commonly known music objects, which are
summarized below [and will be re-explained, and visualized, in the live
session].

@section{Tones}

For our purposes, a tone is a sound which is produced by a sine wave
at a given frequency.  

@codeblock{
<freq>Hz ; Standard Syntax
440Hz ; Example

(tone <freq>) ; Tonart Syntax
(tone 440)  ; Example
}

A special property of how we perceive
tones is: two tones that are double each other's frequency sound "almost the
same" (are hard to tell apart).  This is why we can all sing the same songs
together even though we have different vocal ranges.

@section{Notes}

For our purposes, notes are a standard meter laid over top of tones
(frequencies).  

@codeblock{
;; Standard Syntax
<pitch-label><accidental><octave>
Ab5 ; Example

;; Tonart Syntax
(note <pitch-label> <accidental-number> <octave-number>)
(note a -1 5) ; Example
}

Notes are given labels A1 B1 C1 D1 E1 F1 G1 A2 B2 C2 D2 ...,
which, in that order, must have increasing corresponding frequencies.  C2 must
have a frequency exactly double C1.  We call the distance between two notes an
"interval" and this particular same-sounding, doubled-frequency interval an
"octave".  Two notes an octave apart will always have the same _pitch-label_,
just as in the above example.  The octave number is what will vary (C1, C2, C3,
C4 etc.).  An expanded set of tones can be expressed as a raising or lowering of
a note, which is done via an "accidental".  To raise a note, the symbol '#'
(pronounced "sharp") is used.  To lower a note, the symbol 'b' (pronounced
'flat') is used. The accidental is also preserved between octaves, and the
pitch-label taken with the octave is called the "pitch class".  For example, a
raised C is written C#, each distinct pitch classes.  C# in the 4th octave is
written C#4.  The C# an octave above C#4 is written C#5.  For clairty- C#4 and
C#5 are in the same pitch class and that pitch class is C#.

@section{Tuning}

A tuning is a named precise mapping of notes to tones.  This usually means mapping
one octave of symbols- C #/b D #/b E F #/b G #/b A #/b B to reasonable frequencies,
and deriving all the others by doubling or halving those frequencies
(transposing by octaves).

@codeblock{
;; Standard Syntax
<tuning-name><note>=<frequency>
;; Example
12tet a=440Hz

;; Tonart Syntax
(tuning <tuning-name> [<note> <frequency-number>])
;; Example
(tuning 12tet [a 440])
}

  Many tuning systems exist.   When browsing tuning
systems, recognize that these are typically uniform ways to tune a keyboard,
with compromises based around what kind of music is to be played.  Moreover, a
performance practice for small groups or vocalists (without keyboard
accompaniment) will de facto employ a "context sensitive tuning", as the ear and
instrument are capable of retuning notes to be closer to "more correct"
intervals.  I will not speculate on what "more correct" means here, but I can
safely say that static tuning systems are forced to approximate intended
intervals in many situations.  And that tuning on the whole is not a matter of
some kind of mathematical correctness, it is rather a matter of enhancing
expression.

@section{Chords}

Chords are an abstraction over combinations of notes, which often function like 
a set of pitch classes.  For the purposes of this demo, our chord symbols will
specify sets of 3 pitch classes, called triads.  

@codeblock{
;; Standard Syntax
<pitch-label><accidental><quality-abbrev> or <pitch-class> <quality>
;; Example
F#m or F# minor

;; Tonart Syntax
(chord <pitch-label> <accidental-number> [<quality-abbrev>])
;; Example
(chord f 1 [m])
}

While chords in some music are just an analytical or summary tool with
debatable real existance, for many guitarists chord symbols and strum patterns
are the primary building blocks of their musical language.  Jazz charts are both
summarized in chords and written in chords, though jazz chords typically have 4,
5, 6 or more members in their corresponding pitch class sets, to phrase it in
our established terminology.  

Any number of notes can be in a chord, including repeats and octave transpositions.
Not all pitch classes from the set need be present for a chord to retain its identity, either.
It is important to remember that these are abstractions created to describe the
phenomena we experience.  For example, a pipe organ playing a single notated
note may actually be playing a chord of some variety.  Any instrument sounds a
whole series of tones which we consider to be dominated by the lowest one,
called the fundamental frequency. It is possible to expand the score into one
which reflects some musical reality based on the notes, the instruments, the
chords, the room, the weather that day etc. and then analyze that, but we are
always going to be applying thresholds to determine what is significant.  Anyway
having a toolkit to facilitate this kind of thing is useful.