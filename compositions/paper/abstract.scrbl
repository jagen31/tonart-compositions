#lang scribble/acmart
@require[scriblib/footnote]

@define-footnote[familiar-note make-familiar-note]
@provide[make-familiar-note]

Music composition is an artistic process in which a composer analyzes musical
language from a diverse array of cultures in order to synthesize new works. The
conscientious composer spends significant time experimenting in multi-language
music ecosystems and translating the results of these experiments into
performable or listenable output.  This demo introduces Tonart, an extensible
system for composing and compiling works expressed in multiple music languages. 
Terms from the chosen languages are embedded in a "score", a coordinate system 
abstraction over a physical musical score.  The score is then given semantics 
in terms of a known music language, using a specified sequence of rewrites.  
The score is accessible at all points in the sequence.  Tonart's rewriters
leverage context-sensitivity and locality, corresponding to how notations
interact when written on physical scores.  Crucially, it is the composer's
choice how conflicts in different languages' usual semantics are addressed.
Moreover, using local rewrites, sections of the score may be given
independent semantics.  In this way, Tonart compiles source multi-language
scores into performable target scores, which may be further compiled into
computer performances or human-readable sheet music.
