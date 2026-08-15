# Realizer Preview

A DrRacket plugin that runs a realizer over the edited module's
`program` art and shows the result in a pane on the right.

Two buttons:

- **Scribble** — `scribble-realizer`, rendered as a document (headings,
  section colors, lilypond scores)
- **Strudel ▸** — `program-strudel-realizer`, handed straight to a
  Strudel REPL on localhost

This is a separate package from `drracket-program-preview`, which
previews a scribble `doc` and is untouched by this one. They share no
code — `layout.rkt` and `scribble-preview.rkt` here are copies.

## Install

```bash
raco pkg install --link /Users/jared.gentner/git/tonart-compositions/drracket-realizer-preview
```

Restart DrRacket, then **View → Show Realizer Preview** (`⇧⌘R`).

To pick up code changes to the plugin, `raco make tool.rkt` and restart
DrRacket.

> Both plugins add their own pane, so with both installed you get two
> menu items and can open two panes side by side. Hide whichever you
> are not using.

## What it expects

A module that **provides a `program`**:

```racket
#lang racket
(require (except-in tonart direction transpose-octave)
         (except-in "../scribble/scribble.rkt" insert))
(provide program)

(define-art program
  (@ [(art-section opener)]
     (art-title "Twinkle Twinkle Little Star")
     (bg "#ffec73")
     (music …)))
```

The binding is realized **bare** — nothing is composed in behind your
back. If you want the colors, compose them in yourself:

```racket
(define-art program program-score program-colors)
```

### Per-realizer overrides

When the two outputs want different things, provide
`program-scribble` or `program-strudel`. Each button prefers its own
binding and falls back to `program`:

| button | realizes | falls back to |
|---|---|---|
| Scribble | `program-scribble` | `program` |
| Strudel | `program-strudel` | `program` |

```racket
(provide program program-scribble program-strudel)

(define-art program program-score)

;; only in the document
(define-art program-scribble program program-colors)

;; only in the .strudel
(define-art program-strudel program (rewrite-in-music (instrument "supersaw")))
```

An override **replaces** `program` rather than adding to it — naming
`program` first, as above, is how you build on it. Composing costs a
token; being unable to replace would not be recoverable.

The status line always names the binding that actually ran
(`program-scribble · 19 blocks · 13.8 s`), since with two possible
sources per button, silence about which one it used would be its own
bug.

Nothing in this repo defines `program` yet; the existing files call it
`program-score`. Rather than guess, a module without one says so and
lists what it does provide:

```
program.rkt defines no `program` or `program-strudel`.

The strudel button realizes `program-strudel` if the module provides
one, and `program` otherwise.

This module does provide: program-colors, program-score,
widor-program-music

Rename one of those to `program`, or add a `(define-art program …)`
that composes them.
```

## Strudel

**The generated source is never shown in the pane.** The pane is for
reading the program; a wall of generated JavaScript is not that. The
button realizes and hands the result straight to a Strudel REPL at
`http://localhost:4321` — the port `npm run dev` serves in a strudel
checkout.

Strudel reads its program out of the URL fragment, so no file is
written and nothing is uploaded anywhere; the code travels in the URL.

The port is checked *before* realizing, not after: realizing takes
~10 s and there is nothing to do with the result if nothing is
listening, so you get the dialog immediately instead of at the end.

A strudel run leaves the pane alone, so a document you already
rendered with **Scribble** survives pressing it.

The encoding matches `code2hash` in strudel's `packages/core/util.mjs`:
base64 of the UTF-8 bytes, then percent-encoded. Verified to round-trip
through strudel's own `hash2code` (`decodeURIComponent` → `atob` →
`TextDecoder`), unicode and the `+ / =` of base64 included.

Neither button writes a `.strudel` file. Run your own
`program-strudel.rkt` when you want the artifact on disk.

## How it works

```
 definitions text ──► extract.rkt (subprocess) ──► result ──► layout.rkt ──► text%
                          │
                          ├─ write a driver module beside the source
                          ├─ compile it (this is where realize runs)
                          └─ read back the doc / the strudel string
```

**A `program` is a `define-art`, which is a *syntax* binding**, so it
cannot be pulled out with `dynamic-require` the way a `doc` can, and
`realize` is a macro that has to run at compile time inside a module
that imports the art. So each button writes a small driver module next
to your source, compiles it, and reads the result back:

```racket
#lang scribble/manual
@(require (only-in tonart realize)
          (only-in (file "…/scribble.rkt") scribble-realizer)
          (only-in (file "…/program.rkt") program))
@(realize (scribble-realizer #:title "…" #:numbered? #f) program)
```

Two details there are load-bearing, both learned the hard way:

- **The `@` prefixes.** `#lang scribble/manual` reads its body in text
  mode, so a bare `(require …)` is literal prose — it renders as a
  paragraph instead of being evaluated.
- **Every import is `only-in`.** These modules re-export each other
  heavily: `scribble.rkt` and `program-strudel.rkt` collide on `sa`,
  `tonart` and `scribble.rkt` collide on `transpose-octave`, and the
  hand-written driver scripts in this repo each carry a bespoke
  `except-in` list to cope. Naming the one binding we need sidesteps
  all of it, and hygiene means the realizer's output still resolves
  against *its* scope, not the driver's.

The driver has to be a **sibling** of your source: the realizers
resolve `#:music-dir` and image paths relative to the current
directory, and your own relative `require`s have to keep working. It is
deleted afterward, along with its `.zo`.

`scribble.rkt` and `program-strudel.rkt` are found by walking up from
the edited file for a directory holding both `scribble/scribble.rkt`
and `strudel/program-strudel.rkt` — i.e. `compositions/`.

**Realizing happens in a subprocess.** Expanding one of these programs
shells out to lilypond and takes ~10 s; none of that should be able to
wedge the IDE, and a program that prints to stdout — lilypond does —
must not corrupt the pane. Its output is captured into a build log
shown under any error.

### Working directory

Most compositions build from their own directory, but some reach for
repo-root-relative paths — `compositions/widor/main.rkt` does
`(load-musicxml "compositions/widor/five-one.musicxml" …)`, so widor
only builds from the repo root, and its own `program-strudel.rkt` fails
from its own directory for the same reason.

So a failed run is retried from the repo root (nearest ancestor with a
`.git`), and the answer is remembered per file for the session. If both
fail you get the first error, since the file's own directory is the
usual convention.

## Notes

- The realizer wants a `#:title` and a bare `program` carries none, so
  the composition directory's name is used — `porchfest2026`. Sections
  are unnumbered (`#:numbered? #f`).
- `racket` and `lilypond` are found explicitly, not via `PATH`:
  DrRacket launched from Finder inherits a bare environment with
  neither Homebrew nor `/usr/local` on it.
- **Auto** re-runs **Scribble** ~1.5 s after you stop typing — never
  Strudel, which would open a browser tab per pause. Off by default:
  every run re-runs lilypond, so it is around 10 s a cycle.
