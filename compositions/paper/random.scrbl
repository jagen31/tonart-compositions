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