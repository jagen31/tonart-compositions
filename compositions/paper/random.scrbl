#lang scribble/acmart
@require{references.rkt}
@require[scriblib/footnote]
 
@define-footnote[ptolemy-note make-ptolemy-note]
@define-footnote[reality-note make-reality-note]
@define-footnote[pitch-note make-pitch-note]
@define-footnote[warp-note make-warp-note]
@define-footnote[familiar-note make-familiar-note]

@; start with `title` instead of `section`, because importing via
@;  `include-section` shifts all title/section/subsections down one level
@title{Introduction}

Music occupies an interesting space among fields of study.  Music theories,
even the ones that prop themselves up as being the "theory that finally 
defines musical truth", are incomplete, unsound, not really theories,
and occasionally, not really musical.  And though these theories attempt to
explain broadly the same phenomena, one does not subsume the next in the way of
the natural sciences.  Even in a standard western music education, we still enrich
our music practice by studying Greek metrical and tuning theories, Gregorian
Chant theory, Renaissance hexachord theory, the Thoroughbass and partimento
theory of the baroque era, and many more.  In composition practice, we weave
their concepts together into the same pieces.
@ptolemy-note{Imagine if a physicist was so moved one day to make a calculation using Ptolemy's 
theory of epicycles.  And also, to include it in a published paper! And then 
someone read the paper aloud and everyone clapped at the end.} 
It is a double-edged reality that each music
theory produces a musical output, meaning every single one commands our respect
at some level.  @reality-note{Perhaps the comparison to natural sciences was unnecessary, as
this sentiment is already all-too-familiar for those who study programming
languages.}

@make-ptolemy-note[]
@make-reality-note[]

Alongside music theories, the practice of music notation has unfolded.  Score
notation in particular, originating from the plainchant tradition, has been extended and
extended and extended again to accomodate the needs of theorists, composers, and
performers.  Score paper is the typical medium even for many modern compositions, in a wide
range of styles.  The notations that have emerged are astronomically greater in number and 
creativity, stemming both from academic and non-academic settings.

What has made score notation so extensible?  Well, score notation possesses certain
useful structural features.  Features for immediately locating 1. the
voice/performer an event belongs to and 2. when an event should occur.
@pitch-note{It also gives a visual aid, precise or imprecise, for the pitch of
some events, such as notes. However, I choose to treat this pitch dimension as
part of the notations themselves rather than part of the coordinates.  Some musics such as
spectral music would certainly benefit from a coordinate dealing with pitch, or
a pair of coordinates `pitch` and `pitch-range`.  And perhaps the removal of
time from the coordinate system altogether.  Exploring new coordinate
combinations is highly encouraged, and has proved very beneficial in my own
studies!}

@make-pitch-note[]

Music theories often involve objects which are reasoned about
independently from these structral score features.  For example, in common practice theory,
keys are composed of scale degrees, which can be placed in particular octaves,
and given a particular reference pitch to create notes.  These notes can be
tuned via a tuning system, to particular tones.  None of this concerns space or
time.  @warp-note{This is not true of some objects, which have inherent
spatio-temporal content, that conflicts with or warps the area in which they are
placed.  These objects will come up often in composing, but no clever means of
reasoning about them are given in this paper.  For example, the `tempo` object
distorts the coordinates of the other objects in its region.  I have no sound
means of macro expanding under these conditions yet, other than converting
everything to an equal tempo, doing the desired expansions, and converting back
afterwards.} 
A commonality among most theories is the practice of either inventing notations for objects that embed
directly onto score paper and inherit semantics from their position thereon, or re-expressing
theoretical objects in terms of existing notations.

@make-warp-note[]

First, there is a coordinate system which models a musical score.
Then, the composer selects, invents, reconstitutes, plagiarizes or otherwise
procures a language which can be embedded into this musical score model @familiar-note{The
most familiar language to classical musicians is one consisting of notes, key
signatures, time signatures, dynamics, etc.} Crucially, the forms of the
language may or may not be understood by the intended performer, allowing for a
much higher degree of abstraction and expression.  Next, macros targeting
regions of the score are applied to compile the score into a performable language.
Because of the way syntaxes are embedded into regions, these macros can behave
more like traditional macros or more like "compiler passes", a la nanopass.  A
chain of consecutive macros can be thought of as a "compiler pipeline".
Different pipelines can be applied to the same abstract score to compile into
any number of different, more concrete scores.  Finally, the concrete scores are
"realized" into various artifacts, such as sheet music, or an electronic
performance.

@make-familiar-note[]





Music composition is an artistic process in which a composer analyzes musical
language from a diverse array of cultures in order to synthesize new works.  The
conscientious composer spends significant time experimenting in multi-language
music ecosystems and translating the interesting results of these experiments
into performable or listenable output.  It is my belief, accepting this account
as true, that music computing ought to leverage modern techniques from
extensible language and compiler research to support compositional processes.

This demo introduces Tonart, a system for composing and compiling works
expressed in multiple music languages.  Forms from the languages are embedded in
a musical context which is structurally reminiscent of musical score paper.  The
forms are mutually compiled using a specified sequence of macros.  To do this, 
Tonart transformations leverage a type of
context-sensitivity and locality corresponding to intuitions about how notations
interact when written on score paper.  Taken together, this provides a good set
of abstractions to connect musical experiments to outputs- outputs that "make
sense" by the semantics of extant music languages, but are understood as
images of the Ur-score, so that the original concepts are not "lost in
translation".


Tonart can be broken down into a few parts.

Here is a tone, outside of time.  The `realize' form starts with a subform
describing the realizer to use.  The rest of the forms are written in the
`tonart' language.  This realizer ``draws'' the tonart with a width of 800 and a
height of 200, meaning it converts it to an image from the student library
`2htdp/image' of those dimensions.  There's a simple extensible mechanism for
making it draw new forms, which is a proof of concept but does the job.

@codeblock{
(realize (draw-realizer [800 200]) 
  (tone 440))
}

Here is a tone, in a specific time and voice.  Notice we've changed it to
use the music draw realizer, which treats the art as if it were music.
We've put the tone from beat 0 to 4, and in the soprano voice.

@codeblock{
(realize (music-draw-realizer [800 200]) 
  (@"@" [(interval (start 0) (end 4)) (voice soprano)] 
    (tone 440)))
}


The computer can perform this piece.  Specifically, we can realize it as a sound
in the racket sound library, rsound, and play that using the usual `play' function.  
Notice that we provided a volume as well.  That is a requirement of the rsound realizer.
The music@"@" form is a convenient shorthand for an interval coordinate and a voice coordinate.

@codeblock{
(play
  (realize (music-rsound-realizer) 
    (music@"@" [(0 4) soprano] 
      (tone 440) (volume 4))))
}

However, sopranos do not usually understand how to sing pitches by frequency.
We'll use a note.  We'll scale it's drawing by 2 as well, because we know how that
works now.  The `scale' function is from the `2htdp/image' student library.
This is image scaling, not to be confused with a musical scale!

@codeblock{
(scale 2
  (realize (music-draw-realizer [800 200]) 
    (music@"@" [(0 4) soprano] 
      (note a 0 4) (dynamic mp))))
}

Now we will compile this into a score and an electronic performance.  First,
`define-art' to give it a name.

@codeblock{
(define-art my-music
  (music@"@" [(0 4) soprano] (note a 0 4) (dynamic mp)))
}

Next, realize the electronic performance.

@codeblock{
(define my-music-rsound
  (realize (music-rsound-realizer) my-music (tuning 12tet) (note->tone) (dynamic->volume)))
}

Finally, realize the score, as a musicxml string. `tonart->musicxml' is a rewriter that does
a bunch of syntax transformations of questionable generality to produce a musicxml-ish art that
the musicxml realizer can then take all the way to the proper string form.  Some of the transformations
include using the time signature to group the events into measures, turning the
notes into glyphs representing the notes (with the corresponding duration
symbol), fulfilling the inexplicable design decision of musicxml authors to make every
duration be an integer, and filling empty spaces with rests.

@codeblock{
(define my-music-musicxml
  (realize (music-musicxml-realizer) my-music (time-sig 4 4) (tonart->musicxml)))
}



@section{Tones}

For our purposes, a tone is a sound produced by a regular vibration at a specific frequency.
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
pitch-label taken with the octave is called the "pitch class".  For example, a
raised C is written C#, each distinct pitch classes.  C# in the 4th octave is
written C#4.  The C# an octave above C#4 is written C#5.  For clairty- C#4 and
C#5 are in the same pitch class and that pitch class is C#.

@section{Tuning}

A tuning is a named precise mapping of notes to tones.  This usually means mapping
one octave of symbols- @tt{C #/b D #/b E F #/b G #/b A #/b B} to reasonable frequencies,
and deriving all the others by doubling or halving those frequencies
(transposing by octaves).

@codeblock{
;; Standard Syntax
<tuning-name><note>=<frequency>
;; Example
12tet a=440Hz

;; Tonart Syntax
(tuning <tuning-name> 
  [<note> <frequency-number>])
;; Example
(tuning 12tet [a 440])
}

Many tuning systems exist.  When browsing tuning systems, recognize that
most are uniform ways to tune a @emph{keyboard}.  A performance practice
for a small group without keyboard, or choir without keyboard, will de facto
employ a "context sensitive tuning", as the ear and instrument are capable of
retuning notes to be closer to "more correct" intervals.  I will not speculate
on what "more correct" means here, but I can safely say that static tuning
systems are forced to approximate intended intervals in many situations.  Tuning
on the whole is not a matter of some kind of mathematical correctness, it is
rather a matter of enhancing expression.

@section{Chords}

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
(chord <pitch-label> <accidental-number> [<quality>])
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


In this demo, we will compose a piece of music borrowing ideas from several
different musical and non-musical traditions.  Some of these objects are
unfamiliar to most musicians, so the reader is not expected to be familiar
with them, either.  

The piece will contain a melody, a harmony, and a countermelody.

The melody will be a hymn.  Hymns are songs of worship.  There is a culture of
preserving hymn tunes and texts ranging from Psalms and scripture, to
medieval gregorian chants, to hymns Luther wrote and Bach played in services, to
tunes and texts written by living composers.

The harmony will be specified as a series of neo-riemannian transformations.
Harmony is a characterization of music by the strengths and proportions of the
pitches it contains.  A piece's harmony is made up of a number of harmonies
which are traversed through over time.  The large-scale harmony of an entire
piece is sometimes referred to as "macroharmony".  Neo-riemannian transforms map
out a macroharmony by describing incremental changes to a harmony to construct 
a series of related harmonies.  Our harmony will be realized directly as chords,
which are a simplified representation, essentially a set of a harmony's
most prominent pitches.

The countermelody will be specified as a mathematical function.  The function will map
out the contour of the countermelody, meaning the height of the pitches will follow the
curve of the function's graph.  The pitches will also automatically be selected
to be within the harmony.  The rhythm of the countermelody will be a very basic
interpretation of a Hindu Tala.  Tala is the Indian conception of rhythm and
Indian classical music has names for different Talas, which are rhythmic spaces
that many compositions can take place within.  A crucial part of Tala is each
beat gets a characteristic _word_ in addition to standard timekeeping, which
gives each beat a distinct quality that Indian musicians would be intimately
familiar with.

The demo will proceed by composing each part separately.  We will start with the melody,
write out the harmony, and write a descant (a more intuitive name for this is @emph{countermelody}).

@section{Melody}

The melody will be selected from a hymnal.  Hymnals are books which compile
together a set of hymns, traditional poems and songs.  Hymn technique can be
understood as follows- there are hymn texts and hymn tunes.  Many times tunes
are written for specific texts, but not always.  Tunes and texts are matched by
"meter".  Meter, in this context, is calculated by counting the syllables from
each line of text, and the notes in each section of music.  It is reported the
same way for each, as a series of numbers, separated by dots. 

Tonart contains a hymnal as a library, which is exposed via a SQL-like interface.  
We will pick a tune from the database which has the meter 8.6.8.6.

@codeblock|{
(define-art hymn
  ;; Select all hymns of the intended meter.
  ;; Hymns are decomposed into the rhythm 
  ;; of the hymn and the pitches of the hymn, 
  ;; represented as a sequence of scale 
  ;; degrees. This is a cosmetic interface 
  ;; design  decision, not something 
  ;; fundamental.
  (table@ tonart-hymnal
    (select [rhythm ^s]
      #:where 
        (art-equal (ref meter) (meter 8 6 8 6))
      #:into results))

  ;; Convert the results to a sequence
  (table@ results (table->seq))
  
  ;; inline the first row
  (! 0) (seq-ref) (inline-seq)

  ;; construct the hymn tune from rhythm 
  ;; and pitches.
  (apply-rhythm))

(define-art melody (voice@ melody hymn))
}|

Because the hymns are expressed in scale degrees, we will have to make a choice
of key and octave later.


@section{Harmony}

The harmony will be constructed from neo-riemannian transformations.  These
transformations are, for our purposes, a set of self-inverting chord to chord
transformations.  Some of the ones we will use are

@itemize{
@item{R, for relative, which transforms C major to A minor and back}

@item{L, for leading tone, which transforms C major to E minor and back}

@item{P, for parallel, which transforms C major to C minor and back}

@item{S, for slide, which transforms C major to B minor and back}
 
@item{N, for nebenverwandt, which transforms C major to F minor and back}
}

@codeblock{
;; write down the transforms for the harmony.
;; in converting to chords, an initial chord is 
;; provided, then the list of transforms 
;; notated  in the first subform is applied, 
;; and the second ;; chord is emitted.  Then 
;; the next subform and chord and so on.
(define-art harmony 
  (transforms [R] [L] [N] [P] [L] [P] [S P S]))
}

@section{Descant}

The descant will be composed of a mathematical function, which will provide the
contour for the melody to follow.  The rhythm of the countermelody will be
constructed taking inspiration from a particular system of Indian classical
music called @emph{Taal}.  Indian classical music is more fundamentally rhythmic
than western classical music.  A Taal is a specific rhythmic cycle.  Much like
hymn tunes, popular Tala have names.  We are going to use Theka- a syllabic
representation of the basic rhythm of a Taal.  They can be realized with
gestures like clapping and tapping, but the most common instrument to perform a
Theka on is a Tabla, a pair of hand drums.  Each syllable is rendered as a
different technique on the drum.  The rhythm spelled out by the Theka we choose
will form the rhythm of the descant.  The specific notes of the descant will be
determined by the harmony we established in the last section.

@codeblock|{

(define-art my-theka
  (table@ tonart-tala-index
    (select [theka]
      #:where 
        (art-equal (ref name) (string "arachartal"))
      #:into results))
  
  (table@ results (table->seq))
  (! 0) (seq-ref) (inline-seq))

(define-art the-descant
  (@voice descant
    (function (+ (sin x) (* 2 (sin x))))
    ;; loop the theka every 4 beats for 16 beats
    (i@ [0 16] (loop 4 my-theka) (expand-loop))))
}|

@section{Assembly}

At this point, we have the fragments of our composition named, and just need to
compose them, then render them

@codeblock|{

;; the official composition (as we imagined it)
(define-art composition melody harmony descant)

;; the composition, with all parts compiled 
;; down to notes
(define-art composition-notes

  composition

  ;; specify the key
  (key c 0 major)

  (tuning 12tet)
    
  (voice@ melody (octave 5) (^->note))

  ;; turn the harmony into chords, and notes
  (voice@ harmony 
    ;; provide the first chord relative to the 
    ;; key so it will change when we change the 
    ;; key. This is the analogue of 
    ;; scale degrees, for chords. 
    (^chord 1 0 [M]) (^chord->chord) 
    (run-transformations))
  
  (voice@ descant
    ;; turn the descant function into a bitmap, 
    ;; turn the bitmap into a group of points 
    ;; containing the non-white pixels.
    (function->image) (image->points)

    ;; This is a map I have predefined
    ;; which translates each syllable of a 
    ;; Theka into a "hole" and a volume. The 
    ;; holes will be filled with the notes 
    ;; of the descant, which will give it 
    ;; the correct rhythm.
    theka-volume-map

    ;; use the map to translate the theka
    (interpret-theka)

    ;; turn the chords of the harmony into 
    ;; scales
    (chord->scale)
    ;; take the holes from the theka, the points 
    ;; from the function, and the scales from the 
    ;; harmony and produce notes within the given 
    ;; range of C4 to C5
    (holes+points->notes [c 0 4] [c 0 5]))

  ;; render the harmony as notes in a very 
  ;; basic way.
  (chord->notes/simple 4))

;; the composition further compiled to tones 
;; (for electronic performance)
(define-art composition-for-performance
  
  composition-as-notes

  ;;  Turn it into tones
  (tuning 12tet) (note->tone)

  ;; provide the volumes for the performance
  (@voice melody (volume 9))
  (@voice harmony (volume 3))
  ;; (the descant harmony is already provided
  ;; by the tala interpretation)
  composition-as-notes
  (tuning 12tet) (note->tone))
}|

Lastly, we play the composition.

@codeblock{
  (play (music-rsound-realizer)
    composition-for-performance)
}