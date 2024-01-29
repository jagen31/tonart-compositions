#lang scribble/acmart
@require[scriblib/footnote]

Embedded music programming languages confer a number of benefits to composers by
bringing music objects into the world of modules, bindings, and code reuse. 
However, many composers write music in graphical score editors.  A natural
desire, then, would be to embed music languages into these editors.  A
proper embedding would provide a means to export the score-with-embedded-code
from the score editor to a program in the music language's host language.  
This program ought to be loadable into a larger program context,
leveraging the modular features of the host language to share bindings with
other host language modules.

That is the subject of this demo.  The demo will use the MuseScore score editor
and the Tonart music language embedded in the Racket language, but the same
principles can be applied in other score editors, or with other choices of music
and host language.