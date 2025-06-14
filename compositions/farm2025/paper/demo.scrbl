#lang scribble/acmart
@require{references.rkt}
@require[scriblib/footnote]
@require[(except-in scribble/manual index)
         scribble-math
         scribble-abbrevs/manual (for-label tonart)
         (except-in scribble/eval examples)
         scribble/example
         scribble/bnf
         (only-in 2htdp/image bitmap scale scale/xy)
         (for-syntax racket/base syntax/parse)]

@define-footnote[musescore-note make-musescore-note]
@define-footnote[rights-note make-rights-note]

@title{Demo}
 
@(define helper-eval (make-base-eval))
@interaction-eval[#:eval helper-eval
                  (require art/sequence/ravel (except-in tonart staff-realizer) 2htdp/image "demo.rkt" (except-in "../demo.rkt" room-conf-1)
                           (only-in "demo.rkt" room-conf-1)
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

The approach this demo takes is to imagine the score editor file as a
visualization of a Racket module@~cite[ywiw-icfp-2002] with embedded
Tonart@~cite[tonart-farm-2024].  To embed arbitrary Tonart into the score editor
score, the score's staff text, which is typically used for performance
directions, instead contains Tonart code.  To permit modular features, standard
fields used for accredition, such as "Composer", "Editor", and "Arranger",
contain Racket module system statements.  This allows for importing bindings
from other modules, which are brought into scope in the embedded Tonart code.
Sections of the score editor score can be bound to names.  These bindings are
also in scope in the embedded Tonart, and furthermore, can be exported via the
aforementioned embedded module system statements.  The translation to a Tonart
module is completed by giving Tonart representations for native score editor
elements like notes, rests, key signatures, and section breaks.

@section{Demo Language}

Tonart is an extensible language.  For the demonstration we will use a fixed
Tonart language, containing several notations not natively implemented in MuseScore@musescore-note{https://musescore.org/en}.


Notations denoting music objects are called @emph{objects} in Tonart.
The following objects are defined for the demo language:

@linebreak[]
@(my-bnf 
  (list @nonterm{object}
        @BNF-seq[open @code{note} @nonterm{pitch} @nonterm{accidental} @nonterm{octave} close]
        @BNF-seq[open @code{midi} @nonterm{number} close]
        @BNF-seq-lines[
           (list @BNF-seq[open @code{key} @nonterm{pitch} @nonterm{accidental}])
           (list @hspace[1] @BNF-seq[open @nonterm{quality} close] close)]
        @BNF-seq[open @code{^} @nonterm{number} @nonterm{accidental} close]
        @BNF-seq[open @code{octave} @nonterm{number} close]
        @BNF-seq-lines[
           (list @BNF-seq[open @code{chord} @nonterm{pitch} @nonterm{accidental}])
           (list @hspace[1] @BNF-seq[open @nonterm{quality} close] close)]
        @BNF-seq[open @code{location} @nonterm{room} close]
        @BNF-seq[open @code{sound} @nonterm{filename} close]
        @BNF-seq[open @code{manual} @nonterm{id} close]
        @BNF-seq-lines[
         (list @BNF-seq[open @code{registration}])
         (list @hspace[1] @BNF-seq[open @nonterm{manual} @nonterm{stop} @litchar{...} close] @litchar{...} close)]
        ))
  
@linebreak[]

@code{note}, @code{midi}, @code{chord}, @code{key}, and @code{octave} denote the expected standard music objects.

@code{^} denotes a musical scale degree.

@code{location} denotes a place in which a section will be performed.

@code{sound} denotes a named sound which will be played, such as a birdsong or a door slamming shut.

@code{manual} is a notation from organ music, denoting which keyboard a section should be played on.

@code{registration} is also a notation from organ music, denoting which stops will be pulled in a section.

@linebreak[]

To place notations in a score, Tonart has a special class of notations called
@emph{coordinates}.  One or more coordinate notations can be attached to a
notation to indicate where in the score that notation takes place.
The following coordinates will be used in the demo language:

@linebreak[]
@(my-bnf 
  (list @nonterm{coord}
        @BNF-seq-lines[
         (list @BNF-seq[open @code{interval}])
         (list @hspace[1] @BNF-seq[open @nonterm{number} @nonterm{number} close] close)]
        @BNF-seq[open @code{instant} @nonterm{number} close]
        @BNF-seq[open @code{voice} @kleenestar[@nonterm{id}] close]
        @BNF-seq[open @code{name} @nonterm{id} close]
        ))

@linebreak[]

@code{interval} denotes a range of beats.

@code{instant} denotes an exact beat.

@code{voice} denotes a musical voice.

@code{name} is a shared identifier given to a group of related Tonart notations.
For example, @code{(name ode-to-joy)} may be attached to the notes of
the famous melody from @emph{Beethoven's Ninth Symphony}.

@linebreak[]

In Tonart, coordinates are attached to notations using the @code{@"@"} form:

@linebreak[]
@(my-bnf
  (list @nonterm{tonart-form}
        @nonterm{object}
        @nonterm{coord}
        @BNF-seq-lines[
          (list @BNF-seq[open @code{@"@"} @BNF-seq[open @kleenestar[@nonterm{coord}] close]
                @BNF-seq[@kleenestar[@nonterm{tonart-form}] close]])]))

@linebreak[]

Coordinates are typically used to determine whether one notation is contextually relevant to 
another.

Tonart possesses facilities for rewriting scores, called @emph{rewriters}.  The following rewriters 
will be used in the demo.

@linebreak[]
@(my-bnf
  (list @nonterm{rewriter}
      @BNF-seq[open @code{transpose} @nonterm{number} close]

      @BNF-seq[open @code{^->note} close]
      @BNF-seq[open @code{note->midi} close]
      ))
@linebreak[]

@code{transpose} adds a constant to scale degrees.

@code{^->note} rewrites scale degrees to notes, given a contextual key and octave.

@code{note->midi} rewrites notes to midis, using a standard formula.

Some additional administrative notations and rewriters will be introduced
throughout this document.@rights-note{I will try to use only this language over the course
of the live demo, but reserve the right to introduce additional syntax, should the need for
it arise.}

@make-musescore-note[]
@make-rights-note[]

@section{Demo Score}

We begin with a sample theme, in the score editor.  This uses only MuseScore features.

@linebreak[]
@(scale/xy 2/13 1/5 (bitmap "score_1.png"))
@linebreak[]

We would like to bind the sample theme to a name, to make it reusable.  Naming
is not a feature of MuseScore, so we have to annotate a name via embedding Tonart in the
MuseScore Staff Text.  Here is an example showing some standard uses of
Staff Text:

@linebreak[]
@(scale/xy 2/13 1/5 (bitmap "score_1_5.png"))
@linebreak[]

Staff Text in MuseScore is applied to a specific beat.  To translate this
to Tonart, we use the @code{instant} coordinate.  Text @emph{t} at beat @emph{b} is parsed
as @code{(@"@" [(instant b)] t)}.

However, certain Staff Text applies to a range of music, starting with the beat
the text is on.  For example, ``@emph{Incomprehensibly}'' above applies to the
whole passage.  Meanwhile, ``@emph{wide}'' applies only to the note.  It is
ambiguous whether ``@emph{don't fall down}'' applies to the beat, only, or the
remainder of the passage.  To encode this explicitly, we introduce the
@code{set} and @code{reset} notations, as well as the @code{expand-set}
rewriter, which behave as follows:
@linebreak[]
@elem[#:style "input"]{expandset.tex}
@linebreak[]
Each independent named section of the score module
is punctuated with a MuseScore Section Break.  The Section Break is
interpreted as a @code{reset} when the score editor score is translated to
Tonart, so that the @code{reset} does not have to be explicitly specified.
Therefore, forms which apply only to the beat they are attached to can appear
as-is, while forms which apply to the rest of the passage are wrapped in
@code{set}.

We specify the name for the section, as follows:

@linebreak[]
@(scale/xy 2/13 1/5 (bitmap "score_2.png"))

(the pink symbol on the top right is MuseScore's indication of a Section Break.)

@linebreak[]


To export these names, Racket module syntax goes in the header, as shown:

@linebreak[]
@(scale/xy 2/13 1/5 (bitmap "score_3.png"))
@linebreak[]

We will only include a couple additional sample sections in this document.
The live demo score will include many more.

@linebreak[]

Here is a section which could
augment the theme with information about where the performers would stand,
presumably in a concert hall setting.

@linebreak[]
@(scale/xy 2/13 1/5 (bitmap "score_4.png"))
@linebreak[]

Here is a section which could augment the theme with organ performance information, 
allowing it to be performed on an organ at some point in a composition.

@linebreak[]
@(scale/xy 2/13 1/5 (bitmap "score_5.png"))
@linebreak[]

Lastly, here is a section which constructs a canon using the theme.  It also
includes a pedal line, to add some variety.

@linebreak[]
@(scale/xy 2/13 1/5 (bitmap "score_6.png"))

@linebreak[]

@section{Transforming to Tonart}

Now we would like to translate from MuseScore into Racket and Tonart.  Our
implementation uses MusicXML@~cite[musicxml-2001], as an intermediatary between
the two systems.

Notes are represented in MusicXML like so:

@linebreak[]
@(scale/xy 2/13 1/5 (bitmap "xml1.png"))
@linebreak[]

The embeddings are passed through the @code{direction} element, which is the MusicXML 
representation of Staff Text from MuseScore.

@linebreak[]
@(scale/xy 2/13 1/5 (bitmap "xml2.png"))
@linebreak[]

The module system syntax from the header passes through the @code{credit}
element, which is the MusicXML representation of Composer text, Arranger text, etc.
from MuseScore.

@linebreak[]
@(scale/xy 2/13 1/5 (bitmap "xml3.png"))
@linebreak[]

Using Racket's module system and syntax transformers, we are able to turn the
XML file into a Racket module.  We first read the XML at compile time, extracting the
embedded Racket and Tonart code.  To acheive the desired
semantics, we normalize the Tonart code by applying a few rewrites.  First, we
apply @code{expand-set} to eliminate @code{set} and @code{reset}
from the score.  Next, we run a rewriter called @code{instant->interval}, which
gets rid of all remaining @code{instant} coordinates by translating them into 1 beat
intervals.  Finally, recall that the names we wrote in the score were not
attached using @code{@"@"}.  In order to get the names correct, we run a
rewriter called @code{attach-coordinates}, which takes unattached coordinates
that are written in a score, and attaches them to notations , using the following
rule:
@linebreak[]
@elem[#:style "input"]{attachcoords.tex}
@linebreak[]

After applying this rewrite, our notations now have their respective names attached.

Following these rewrites, the final step is to realize the named notations into Racket definitions.
This is done using the realizer @code{namespace-define-realizer}.  This realizer takes a score with
named notations, and turns it into a series of Racket definitions.

For each set of notations @emph{N} ... with attached name @emph{n}, the realizer
generates one Racket define form, @code{(define-art n N ...)}.
@code{define-art} defines a Racket binding which can be used in embedded Tonart
programs.  These definitions are combined with the module system statements from
the score editor score, completing the translation.

After requiring the score module, we can use the bindings from the score editor
score.  Here is a rendering of the sample theme composed with the location
configuration using the Tonart @code{staff-realizer}.

@(examples
  #:eval helper-eval
  #:label #f
  #:no-prompt
  (realize (staff-realizer) 
    theme1 room-conf-1))

@section{Finale}

For the remainder of the demo, an environment featuring prepared MuseScore and
text modules will be showcased.  The scores will be written in the demo
language, and the showcase will be a small improvisatory performance achieved by
writing Tonart in a REPL environment, with the demo environment loaded.

@section{Limitations and Future Directions}

@define-footnote[musicxml-note make-musicxml-note]

While embedding into an existing score editor is great 
for accesibility, it still leaves much to be desired vis-à-vis interoperation
between host language and embedded language.  The score editor score can only be
loaded as a module, not embedded directly inside other code.  Moreover, it seems
impossible to report errors on the original syntax, as, in our case, MuseScore
and Racket have no means to communicate such error reporting information with
each other.  Finally, the extra intermediary form of MusicXML can be a hassle
to manage@musicxml-note{In the current implementation, many errors are actually reported
with source locations coming from @emph{inside the MusicXML}. This is somewhat
impressive, but misses the mark from a useability perspective.}

@subsection{Interactive Visual Syntax}
Interactive visual syntax@~cite[visual-2020] allows for creating interactive,
embeddable graphical elements that still manage to leverage the bindings and
scope of the host language, just as successful textual embeddings do.  Visual
Syntax additionally provides a mechanism for graphically reporting errors.  On
the one hand, integrating with an existing score editor is appealing, by
enabling existing workflows and reusing years of score editor specific work done
by others.  On the other hand, a bespoke visual syntax or syntaxes would be a
powerful tool for providing that familiar interface, while retaining the
benefits of a textual embedding, and getting rid of intermediate
representations.

@subsection{Output-Directed Programming}
In creating the translation from MuseScore to Tonart, we ended up implicitly
defining a relation between elements of the MuseScore score and Tonart
notations.  Output-Directed Programming uses this relation in both directions
and in a live fashion.  @emph{Sketch-n-Sketch}@~cite[sketch-2019] is an
Output-Directed editor for SVGs.  It allows both graphical and
textual editing.  Edits to the grahpic reflect immediately in the graphic's
domain-specific source code, and edits to the source code immediately reflect on
the graphic.  It is once again hard to imagine such a technology working with an
existing score editor.  That does not change the fact that an Output-Directed
music editor would minimize intermediate representations, maximize feedback, and
be capable of providing well-correlated graphical error messages.

@make-musicxml-note[]