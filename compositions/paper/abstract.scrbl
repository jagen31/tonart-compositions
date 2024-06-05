#lang scribble/acmart
@require[scriblib/footnote]

@define-footnote[familiar-note make-familiar-note]
@provide[make-familiar-note]

This demo introduces Tonart, a language and metalanguage for practical music
composition.  The object language of Tonart is abstract syntax modeling a traditional
musical score.  It is extensible- composers choose or invent syntaxes which
will most effectively express the music they intend to write.  Composition
proceeds by embedding terms of the chosen syntaxes into a coordinate system that
corresponds to the structure of a physical score.  Tonart can easily be written
by hand, as existing scores are a concrete syntax for Tonart.  The metalanguage
of Tonart provides a means of compiling Tonart scores via sequences of rewrites.
Tonart's rewrites leverage context-sensitivity and locality, modeling how
notations interact on traditional scores.  Using metaprogramming, a composer can
compile a Tonart score with unfamiliar syntax into any number of performable
scores.  

In this demo, we will build a Tonart by adding standard music objects and
rewriters one by one.  We will compile all objects into a score paper
representation, as well as a computer performance.  We will add an unexpected
object at the end, and use our creativity to compile it down to something
performable.