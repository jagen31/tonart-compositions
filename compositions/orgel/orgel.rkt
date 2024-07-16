#lang racket

(require pyffi)

(initialize)
(post-initialize)

(run* "import mido")
(run "msg = mido.Message('note_on', (60, 70))")
(run "mido.send(msg)")