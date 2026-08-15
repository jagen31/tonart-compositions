#lang at-exp racket

;; ----------------------------------------------------------------
;; program-strudel-realizer
;;
;; Renders a programmart program (the same shape as
;; `pf:program-score` — `art-section` name-coords + `(music …)`
;; embeddings + `(script …)` embeddings + `art-title`, `image`,
;; `bg`/`fg`) into a single Strudel (.strudel) document, with one
;; named pattern per section voice and an embedded Hydra
;; visualizer block.  The intent is to give the live coder
;; section-shaped affordances:
;;
;;   - each section's music becomes `const <section>_<voice> = stack(...).slow(N)`
;;   - each section has a live-editable gain knob: `let <section>_g = 1`
;;   - each section's `$:` play line consults that knob, so muting
;;     a section is a one-character edit
;;   - per-section Hydra scene functions are emitted (commented
;;     out — last `out()` wins, so the user uncomments the one
;;     they want to drive the visuals)
;;
;; This is *intentionally* a copy of (parts of)
;; `music-strudel-realizer` from `strudel.rkt` rather than a
;; modification: the original keeps working for single-piece
;; `(loop N …)` use, while this one is for programmart-style
;; multi-section scores.
;; ----------------------------------------------------------------

(require (except-in tonart transpose-octave)
         (except-in "../scribble/scribble.rkt" insert)
         ;; brings the `arm-diagram` / `facing` / `full-arm-diagram`
         ;; identifiers into scope so the `{~literal arm-diagram}`
         ;; etc. patterns below free-identifier=? match the same
         ;; identifiers placed in the score by program.rkt.
         "../lilypond/arm-diagram.rkt"
         ;; lilypond's `image` art-object — distinct from
         ;; scribble.rkt's own `image` (which scribble.rkt explicitly
         ;; `except-in`s from lilypond), so we import it under
         ;; `ly-image` to pattern-match the image forms that
         ;; `arm-diagrams->images` leaves behind in each music block.
         (rename-in (only-in "../lilypond/lilypond.rkt" image)
                    [image ly-image])
         (for-syntax syntax/parse racket/string racket/format
                     racket/match racket/list racket/dict racket/path
                     racket/class racket/draw racket/file racket/set
                     racket/math
                     (only-in art/private/core get-id-ctxt context-ref)
                     art/coordinate/name
                     ;; `make-dancer` is the actual 2htdp/image of a
                     ;; dance pose; `save-image` writes any 2htdp/image
                     ;; to disk as a PNG.  Both are needed at
                     ;; macro-expansion time so the realizer can emit
                     ;; pre-rendered pose PNGs alongside the strudel
                     ;; document.
                     "../dance/dance-annotation.rkt"
                     (prefix-in im: 2htdp/image)))

(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-for-syntax (sa . xs) (apply string-append (map ~a xs)))

;; --- coordinate access helpers ---------------------------------

;; expr-art-section: lifted out of the scribble realizer's
;; private name-coordinate machinery so we don't have to re-export
;; it from scribble.rkt.  Just walks the id-ctxt for the
;; `(art-section …)` coord and returns its name components as a
;; syntax list.
(define-for-syntax (expr-art-section stx)
  (syntax-parse (context-ref (get-id-ctxt stx) #'art-section)
    [({~datum art-section} n ...) (syntax->list #'(n ...))]
    [_ '()]))

(define-for-syntax (section-path stx)
  (map syntax->datum (expr-art-section stx)))

;; Pull the variant label off a dance-related expr's id-ctxt.  The
;; user opts a music block into multi-variant dances by wrapping
;; each choreography in `(name@ <variant> …)` — the standard art
;; rewriter that pins a `(name <variant>)` coord onto its body —
;; so a pose ends up with `(name a)` (or `(name b)`, etc.) in its
;; id-ctxt.  We pull the FIRST name symbol and use it as the
;; variant key downstream; everything not in a `name@` scope (or
;; merely doubly-nested deeper) shares the unnamed bucket `#f`.
;; The unnamed bucket is what existing single-variant scores
;; produce, so this stays backward-compatible — they get a single
;; `_default` variant in the emitted JS wrapper.
(define-for-syntax (expr-variant-sym e)
  (define ctx (get-id-ctxt e))
  (and ctx
       (let ([n (context-ref ctx #'name)])
         (and n
              (syntax-parse n
                [({~literal name} sym . _) (syntax->datum #'sym)]
                [_ #f])))))

;; --- naming ----------------------------------------------------

;; "Look mom!" → "Look_mom"; symbols and digits are kept verbatim,
;; everything else collapses to a single underscore.  Used to
;; build js identifiers from art-section name segments.
(define-for-syntax (sanitize-id sym)
  (define s (cond [(symbol? sym) (symbol->string sym)]
                  [(string? sym) sym]
                  [else (~a sym)]))
  (define cleaned (regexp-replace* #rx"[^A-Za-z0-9]+" s "_"))
  (if (zero? (string-length cleaned)) "x" cleaned))

;; section path '(look-mom twinkle) → "look_mom_twinkle".  Empty
;; path (top-level / no section) → "_main".
(define-for-syntax (section-path->id path)
  (cond [(null? path) "_main"]
        [else (string-join (map sanitize-id path) "_")]))

;; voice coord → js identifier suffix.  No voice → "voice".
(define-for-syntax (voice-id-for note)
  (define vs (expr-voice note))
  (cond [(null? vs) "voice"]
        [else (sanitize-id (syntax->datum (car vs)))]))

;; --- per-note → strudel timecat fragment -----------------------

;; Replicates the per-note conversion in music-strudel-realizer
;; (strudel.rkt).  Returns one timecat-string for `note-stx`
;; placed at `(start+offset)..(end+offset)` inside a section of
;; total length `section-len`.  When a section has several
;; back-to-back `(music …)` blocks (e.g. swallowtail-jig's A and
;; B parts), `offset` is the cumulative duration of preceding
;; blocks, so the blocks concatenate in time instead of stacking
;; at 0.  `inner-ctxt` is the list of exprs inside the `(music
;; …)` block — used to look up the per-music `(instrument …)` /
;; `(volume …)` set by `rewrite-in-music`.
(define-for-syntax (note->timecat note-stx offset section-len inner-ctxt)
  (syntax-parse note-stx
    [({~literal note} p a o)
     ;; Look for `(instrument …)` and `(volume …)` inside the
     ;; *music body* — that's where `(rewrite-in-music
     ;; (instrument "…"))` injects them.  Using the outer
     ;; `(current-ctxt)` here would miss anything scoped inside
     ;; the `(music …)` embedding.
     (define/syntax-parse {~or (_ name) name}
       (get-context inner-ctxt note-stx #'instrument))
     (define/syntax-parse {~or (_ vol) vol}
       (get-context inner-ctxt note-stx #'volume))
     (define start (+ offset (expr-interval-start note-stx)))
     (define end   (+ offset (expr-interval-end   note-stx)))
     (define inst-str
       (if (syntax-e #'name)
           (format ".s(~s)" (syntax-e #'name))
           ".s(\"square\")"))
     (define vol-str
       (if (syntax-e #'vol)
           (format ".gain(~s)" (syntax-e #'vol))
           ""))
     (define acc-str
       (match (syntax-e #'a)
         [0 ""] [1 "#"] [-1 "b"] [2 "x"] [-2 "bb"]
         [3 "###"] [4 "####"] [-3 "bbb"] [-4 "bbbb"]))
     (define lead (if (zero? start) "" (format "[~a, silence], " start)))
     (define tail (if (= end section-len) ""
                      (format ", [~a, silence]" (- section-len end))))
     @sa{timecat(@|lead|[@(- end start), note("@(syntax-e #'p)@|acc-str|@(syntax-e #'o)")@|inst-str|@|vol-str|]@|tail|)}]))

;; --- per-music block processing --------------------------------

;; Lightweight raw record returned by `music->raw-entry` — keeps
;; the note syntax + their inner-ctxt around so we can render
;; timecats later, once we know the section's total length and
;; this block's time offset within it.
(begin-for-syntax
  (struct music-block [length voice->notes inner-ctxt] #:prefab))

;; Given a single `(music inner ...)` expr from `(current-ctxt)`,
;; returns a `music-block` or #f if the block has no notes
;; (key/time-sig/divisions only).
(define-for-syntax (music->raw-entry music-expr)
  (syntax-parse music-expr
    [({~literal music} inner ...)
     (define inner-ctxt (syntax->list #'(inner ...)))
     (define notes
       (filter (λ (e) (syntax-parse e
                        [({~literal note} _ _ _) #t]
                        [_ #f]))
               inner-ctxt))
     (cond
       [(null? notes) #f]
       [else
        (define len
          (for/fold ([m 0]) ([n (in-list notes)])
            (with-handlers ([exn:fail? (λ _ m)])
              (max m (expr-interval-end n)))))
        (define len* (if (zero? len) 1 len))
        (define voice->notes
          (for/fold ([h (hash)]) ([n (in-list notes)])
            (hash-update h (voice-id-for n) (λ (xs) (cons n xs)) '())))
        ;; sort each voice's notes by start so timecats come out
        ;; in time order
        (define voice->sorted
          (for/hash ([(v ns) (in-hash voice->notes)])
            (define sorted
              (sort ns < #:key (λ (n) (with-handlers ([exn:fail? (λ _ 0)])
                                        (expr-interval-start n)))))
            (values v sorted)))
        (music-block len* voice->sorted inner-ctxt)])]))

;; Read the `(divisions N)` context entry out of a single music
;; block.  Returns the integer N if the block has a `(divisions …)`
;; marker, otherwise #f.  Inline music blocks (notes written by
;; hand, no musicxml import) typically lack one — callers should
;; treat #f as "this block doesn't pin the rate" and fall back to a
;; sibling block that does (see `blocks->divisions`).
(define-for-syntax (music-block->divisions music-expr)
  (syntax-parse music-expr
    [({~literal music} inner ...)
     (define ic (syntax->list #'(inner ...)))
     (define d (context-ref ic #'divisions))
     (cond [d (syntax-parse d
                [({~literal divisions} n:number) (syntax-e #'n)]
                [_ #f])]
           [else #f])]))

;; A section's divisions = the divisions of the FIRST block that
;; explicitly declares one.  Looking only at the first block was a
;; bug when an art-section interleaved an inline placeholder
;; `(music (note …))` (no divisions) with a musicxml-imported
;; `(music drop)` (divisions=N) — the fallback `1` then propagated
;; through the dance-strip math and produced a bitmap N× too wide.
;; Scanning blocks-in-order keeps the original "first block wins"
;; semantics for normal cases (where every block carries the same
;; divisions) while making inline-then-imported work correctly.
;; All-inline sections still default to 1 (the musicxml convention
;; when `<divisions>` is absent: one duration-unit = one quarter).
(define-for-syntax (blocks->divisions blocks)
  (or (for/or ([b (in-list blocks)]) (music-block->divisions b))
      1))

;; Combine a section's music blocks into per-voice timecat lists,
;; concatenating blocks in authoring order along the time axis.
;; Returns (values voice->timecats-hash total-length).
(define-for-syntax (blocks->voice->tc blocks)
  (define raws (filter values (map music->raw-entry blocks)))
  (define total
    (cond [(null? raws) 1]
          [else (for/sum ([b (in-list raws)]) (music-block-length b))]))
  (define-values (acc _)
    (for/fold ([h (hash)] [offset 0]) ([b (in-list raws)])
      (define ic (music-block-inner-ctxt b))
      (define h2
        (for/fold ([h h]) ([(v notes) (in-hash (music-block-voice->notes b))])
          (define tcs (map (λ (n) (note->timecat n offset total ic)) notes))
          (hash-update h v (λ (xs) (append xs tcs)) '())))
      (values h2 (+ offset (music-block-length b)))))
  (values acc total))

;; --- script → js comment block ---------------------------------

(define-for-syntax (frag->plain f)
  (syntax-parse f
    [s:str (syntax-e #'s)]
    [({~literal redact} t:str) (make-string (string-length (syntax-e #'t)) #\█)]
    [_ "?"]))

(define-for-syntax (script-comment script-expr)
  (syntax-parse script-expr
    [({~literal script} inner ...)
     (define lines
       (for/list ([item (in-list (syntax->list #'(inner ...)))])
         (syntax-parse item
           [({~literal line} ch:id frag ...)
            (format "//   ~a: ~a"
                    (string-upcase (symbol->string (syntax-e #'ch)))
                    (apply string-append
                           (map frag->plain (syntax->list #'(frag ...)))))]
           [({~literal stage-direction} frag ...)
            (format "//   ( ~a )"
                    (apply string-append
                           (map frag->plain (syntax->list #'(frag ...)))))]
           [_ #f])))
     (string-join (filter values lines) "\n")]))

;; --- dance: per-music-block arm-diagram extraction & png emit ---
;;
;; For each `(music …)` block we scan its inner-ctxt for two kinds
;; of art objects, exactly mirroring `compositions/lilypond/arm-diagram.rkt`:
;;
;;   - `(facing dir)`      — a time-spanning orientation (towards,
;;                           away, left, right)
;;   - `(arm-diagram l r)` — a pose at a particular instant,
;;                           consulting the surrounding `facing`
;;                           for orientation
;;
;; A pose is identified by the triple `(l r facing-sym)`; each
;; unique triple is rendered once to a PNG on disk so the strudel
;; pattern below can show it with `s0.initImage(…)`.

;; Write a single dance-pose PNG to disk.  Skips if file already
;; exists so re-realizes don't repaint identical poses.
(define-for-syntax (render-dance-png! l r facing-sym disk-path)
  (unless (file-exists? disk-path)
    (define img (make-dancer l r facing-sym))
    (im:save-image img (path->string disk-path))))

;; Per-beat cell size in pixels for both per-pose PNGs (square) and
;; the playback strip (one cell per beat of the dance's cycle).  Big
;; enough that arms (±110 px from body center) never get cropped, with
;; a comfortable margin.
(define-for-syntax POSE-SIZE 280)

;; Center a single dance pose on a transparent POSE-SIZE square so
;; per-pose PNGs all share a uniform bounding box.
(define-for-syntax (uniform-pose-image l r facing-sym)
  (im:overlay/align "middle" "middle"
                    (make-dancer l r facing-sym)
                    (im:rectangle POSE-SIZE POSE-SIZE 'solid (im:color 0 0 0 0))))

;; Build (and write) a section's playback STRIP PNG.  Two layouts,
;; selected by `mode`:
;;
;; 'rhythm (default — paired with constant scroll on the JS side):
;;   Strip width = cycle × POSE-SIZE; each pose is painted full
;;   size, CENTERED at pixel `start × POSE-SIZE`.  Pose-to-pose
;;   spacing on the strip is proportional to the rhythm, so when
;;   the strip scrolls at a constant rate the visual cadence
;;   matches the music's cadence — short-rhythm passages feel
;;   busy/cramped, long-rhythm passages have lots of empty space.
;;   Adjacent poses with rhythm < 1 beat overlap on the strip;
;;   we paint in time order so the later pose ends up on top in
;;   the overlap region (so as the bar enters a new pose's
;;   territory, it sees that new pose right away).
;;
;; 'uniform (paired with variable scroll on the JS side):
;;   Strip width = N × POSE-SIZE (one cell per frame, uniform
;;   spacing).  Each pose is painted full size at cell center.
;;   No overlaps, every pose gets the same on-screen footprint;
;;   the music's rhythm shows up as a varying scroll speed instead
;;   (classic Guitar Hero chart-lane behaviour).
;;
;; In both modes, every pose comes out the SAME native size on
;; disk — no shrinking short-rhythm poses (which used to make the
;; parade look like "random sizes").
;;
;; `frames` is a list of `(list start dur l r facing-sym)` in beats.
;; `cycle` is the dance cycle length in beats (used by 'rhythm mode
;; only — 'uniform mode just uses (length frames)).
;; Returns #t if the file was actually written (or already on
;; disk), #f if nothing to render.
;;
;; We DON'T skip when the PNG already exists on disk: the strip is a
;; pure function of (frames, cycle, mode), and any of those can
;; change between realizer runs (e.g. extending the dance from 8 to
;; 35 frames).  A stale cache file would make the JS-side scroll
;; math — which reads `cycle` straight from the realizer's output —
;; disagree with the texture's actual cell layout, producing the
;; classic "poses are N× farther apart than they should be" bug
;; (where N = new-cycle / cached-cycle).  Repainting is cheap (a
;; few hundred 2htdp/image place-image calls), so just always
;; redraw and overwrite.
(define-for-syntax (render-section-strip! frames cycle mode disk-path)
  (cond
    [(null? frames) #f]
    [else
     (define strip-w
       (case mode
         [(rhythm)  (max 1 (exact-round (* cycle POSE-SIZE)))]
         [(uniform) (* (length frames) POSE-SIZE)]
         [else (error 'render-section-strip! "unknown mode ~a" mode)]))
     (define cy (exact->inexact (/ POSE-SIZE 2)))
     ;; Wrap-padding offsets: every pose gets painted at its main
     ;; center pixel and ALSO at ±strip-w.  For poses comfortably
     ;; inside the strip, the ± copies fall fully off-canvas and
     ;; get clipped (free no-op).  For poses straddling the left
     ;; edge (start ≈ 0 ⇒ center ≈ 0, left half at negative
     ;; pixels), the `+strip-w` copy paints that lost left half
     ;; at the RIGHT edge of the strip — exactly where GL_REPEAT
     ;; samples from when the scroll wraps source-uv past 1 back
     ;; to 0.  Symmetric on the right edge.  Without this, the
     ;; first pose appears chopped in half at phase 0 and the
     ;; wrap region shows whatever pose got painted at the strip
     ;; boundary.  'uniform mode keeps every cell ≥ POSE-SIZE/2
     ;; from the strip edges, so the wrap copies never have any
     ;; visible content there — but they're cheap, and emitting
     ;; them unconditionally keeps the code path uniform across
     ;; modes.
     (define base (im:rectangle strip-w POSE-SIZE 'solid (im:color 0 0 0 0)))
     (define final
       (for/fold ([img base]) ([f (in-list frames)] [i (in-naturals)])
         (match-define (list start _dur l r face) f)
         (define pose-img (make-dancer l r face))
         (define center-x
           (case mode
             [(rhythm)  (* start POSE-SIZE)]
             [(uniform) (* (+ i 0.5) POSE-SIZE)]))
         (for/fold ([img img]) ([dx (in-list (list 0 (- strip-w) strip-w))])
           (im:place-image pose-img
                           (exact->inexact (+ center-x dx))
                           cy
                           img))))
     (im:save-image final (path->string disk-path))
     #t]))

;; Find the `(facing dir)` art-object that covers `t` (start time
;; ≤ t < end time).  Returns the symbol or 'towards as a default.
(define-for-syntax (facing-at facings t)
  (or (for/or ([f (in-list facings)])
        (define f-start (with-handlers ([exn:fail? (λ _ +inf.0)])
                          (expr-interval-start f)))
        (define f-end   (with-handlers ([exn:fail? (λ _ -inf.0)])
                          (expr-interval-end f)))
        (and (<= f-start t) (< t f-end)
             (syntax-parse f
               [({~literal facing} name:id) (syntax-e #'name)]
               [({~literal facing} (name:id)) (syntax-e #'name)])))
      'towards))

;; Pose filename regex — matches both `arm-6-6-towards.eps`
;; (written by lilypond/arm-diagram.rkt's arm-diagrams->images
;; rewriter, which has typically already run by the time the
;; strudel realizer sees its `(music …)` blocks) and bare
;; `arm-diagram` / `full-arm-diagram` art objects.  Captures
;; (l, r, facing-symbol).
;; A single arm-position field in a pose filename.  Accepts integers
;; (`6`, `-3`), decimals (`4.5`, `10.5`) and `/`-fractions (`11/2`).
;; `arm-diagrams->images` writes filenames via `format "~a"`, which
;; prints whatever the source wrote: an integer literal stays integer,
;; a decimal literal stays decimal, an exact rational comes out as
;; `n/d`.  All three forms have to round-trip through this regex or
;; the frame gets silently dropped on the strudel side (no PNG cell,
;; no JS onset) — which manifests as missing dancers in the parade.
(define-for-syntax POSE-FILENAME-RX
  #rx"arm-(-?[0-9]+(?:[./][0-9]+)?)-(-?[0-9]+(?:[./][0-9]+)?)-([a-zA-Z]+)\\.[a-zA-Z]+$")

;; Read "6", "4.5", "11/2", "-3", etc. as an exact rational.
(define-for-syntax (str->rat s)
  (cond [(regexp-match? #rx"/" s)
         (define parts (regexp-split #rx"/" s))
         (/ (string->number (car parts)) (string->number (cadr parts)))]
        [else (string->number s)]))

;; Walk a music-block's inner-ctxt and return a hash
;;   variant-sym-or-#f → sorted list of pose-frames
;;     (list start end l r facing-sym)
;; Recognises three sources:
;;
;;   1. `(arm-diagram l r)`  — pose only, look up facing from a
;;                              surrounding `(facing dir)`
;;   2. `(full-arm-diagram l r dir)` — pose + facing baked in
;;   3. `(image "…/arm-l-r-dir.eps")` — the rewritten form that
;;      `lilypond/arm-diagram.rkt`'s `arm-diagrams->images`
;;      leaves behind once it has run.  We pull l/r/facing back
;;      out of the filename so the strudel side can drive the
;;      same dance even though the bare art-objects are gone.
;;
;; Each pose is bucketed by its variant tag (the `name@` symbol
;; on its id-ctxt; `#f` for poses not under any `name@`).
;; Facings are bucketed the same way and looked up per-variant —
;; an `(arm-diagram l r)` in variant a only consults variant a's
;; facings, with the unnamed bucket acting as a fallback so a
;; score can keep a single `(facings …)` that applies to all
;; variants.
;;
;; Poses with non-real start times are dropped (they don't
;; belong on a timeline).
(define-for-syntax (music-block->raw-poses inner-ctxt)
  (define facings
    (filter (λ (e) (syntax-parse e
                     [({~literal facing} _) #t]
                     [({~literal facing} _ _) #t]
                     [_ #f]))
            inner-ctxt))
  (define facings-by-variant
    (for/fold ([h (hash)]) ([f (in-list facings)])
      (define v (expr-variant-sym f))
      (hash-update h v (λ (xs) (cons f xs)) '())))
  (define (facings-for variant)
    (append (hash-ref facings-by-variant variant '())
            (if variant (hash-ref facings-by-variant #f '()) '())))
  (define (parse-image-pose e)
    (syntax-parse e
      ;; both `ly-image` (lilypond's image art-object, what
      ;; `arm-diagrams->images` actually emits) and the scribble
      ;; `image` (which a hand-authored score might place
      ;; directly) — pull the path out, then peek at the filename.
      [({~or {~literal ly-image} {~literal image}} path:str)
       (define m (regexp-match POSE-FILENAME-RX (syntax-e #'path)))
       (cond [m
              (define l (str->rat (list-ref m 1)))
              (define r (str->rat (list-ref m 2)))
              (define face (string->symbol (list-ref m 3)))
              (list l r face)]
             [else #f])]
      [_ #f]))
  (define diagrams
    (filter values
            (for/list ([e (in-list inner-ctxt)])
              (define direct
                (syntax-parse e
                  [({~literal arm-diagram} _ _) e]
                  [({~literal full-arm-diagram} _ _ _) e]
                  [_ (and (parse-image-pose e) e)]))
              direct)))
  (define raw
    (for/list ([d (in-list diagrams)])
      (define variant (expr-variant-sym d))
      (define start (with-handlers ([exn:fail? (λ _ #f)])
                      (expr-interval-start d)))
      (define end (with-handlers ([exn:fail? (λ _ start)])
                    (expr-interval-end d)))
      (cond
        [(or (not start) (not (real? start)) (infinite? start)) #f]
        [else
         (define entry
           (cond
             [(parse-image-pose d)
              => (λ (lrf) (list* start end lrf))]
             [else
              (syntax-parse d
                [({~literal full-arm-diagram} l:number r:number f:id)
                 (list start end (syntax-e #'l) (syntax-e #'r) (syntax-e #'f))]
                [({~literal arm-diagram} l:number r:number)
                 (list start end (syntax-e #'l) (syntax-e #'r)
                       (facing-at (facings-for variant) start))])]))
         (cons variant entry)])))
  (define by-variant
    (for/fold ([h (hash)]) ([item (in-list (filter values raw))])
      (define v (car item))
      (define entry (cdr item))
      (hash-update h v (λ (xs) (cons entry xs)) '())))
  (for/hash ([(v xs) (in-hash by-variant)])
    (values v (sort xs < #:key car))))

;; Combine all music blocks in a section into per-variant
;; timelines of dance frames.  Returns a hash mapping each
;; variant tag (a symbol from a `name@` wrapper, or `#f` for the
;; unnamed/default bucket) to (cons frames total-len) — frames
;; is a list of (list start dur l r facing-sym) entries.  `dur`
;; runs from this pose's start until the NEXT pose's start
;; (within the same variant) or to the section's total length
;; for the last pose, clipped by the pose's own placed interval
;; (so a `(i@ [k k+2] (arm-diagram …))` pose holds for 2 units
;; even when it's the last frame, instead of stretching to fill
;; the whole section).  All variants share the same total-len
;; (the section's music timeline).
(define-for-syntax (blocks->dance-frames blocks)
  (define-values (variant->acc total)
    (for/fold ([h (hash)] [offset 0]) ([m (in-list blocks)])
      (syntax-parse m
        [({~literal music} inner ...)
         (define inner-ctxt (syntax->list #'(inner ...)))
         (define poses-by-v (music-block->raw-poses inner-ctxt))
         ;; block-len anchors the timeline for the LAST pose's dur
         ;; (dance-frames computes dur=next-start-start, with
         ;; block-len playing the role of "next start" for the very
         ;; last pose).  Prefer the notes' extent; if this is a
         ;; notes-free "dance only" block (e.g. baroque-beats-master),
         ;; fall back to the max pose end so the last pose's dur is
         ;; sane instead of a huge negative number.
         (define block-len
           (let ([raws (filter values (list (music->raw-entry m)))])
             (cond
               [(cons? raws) (music-block-length (car raws))]
               [else
                (define pose-ends
                  (for/fold ([acc '()]) ([(_v ps) (in-hash poses-by-v)])
                    (append (map cadr ps) acc)))
                (cond [(null? pose-ends) 1]
                      [else (apply max pose-ends)])])))
         (define h2
           (for/fold ([h h]) ([(v poses) (in-hash poses-by-v)])
             (define shifted
               (for/list ([p (in-list poses)])
                 (list* (+ offset (car p))
                        (+ offset (cadr p))
                        (cddr p))))
             (hash-update h v (λ (xs) (append xs shifted)) '())))
         (values h2 (+ offset block-len))])))
  (for/hash ([(v acc) (in-hash variant->acc)])
    (define starts (map car acc))
    (define next-starts
      (cond [(null? starts) '()]
            [else (append (cdr starts) (list total))]))
    (define frames
      (for/list ([p (in-list acc)] [ns (in-list next-starts)])
        (define start (car p))
        (define end   (cadr p))
        (define gap   (- ns start))
        (define own   (- end start))
        (define dur   (cond [(positive? own) (min gap own)]
                            [else gap]))
        (list* start dur (cddr p))))
    (values v (cons frames total))))

;; Format a rational as JS source.  Strudel parses `0.5` and `2`
;; fine but chokes on `1/2`.
(define-for-syntax (num->js x)
  (cond [(integer? x) (~a x)]
        [(rational? x) (~a (exact->inexact x))]
        [else (~a x)]))

;; "<l>-<r>-<facing>" — the pose tag we put in the strudel pattern
;; and in the PNG filename.
(define-for-syntax (pose-tag l r f)
  (format "~a-~a-~a" l r f))

;; Build the JS payload for one section's dance: a `<sid>_dance()`
;; constructor that returns a hydra render node.  scene-body calls
;; it as `<sid>_dance().out(o0)`.
;;
;; Strategy: per-section PLAYBACK STRIP, one strip-PNG-pair per
;; CHOREOGRAPHY VARIANT.  All frames of a single variant are
;; pre-baked into a single horizontal strip PNG (cycle × POSE-SIZE
;; px wide, POSE-SIZE tall — each frame's pose painted at its
;; scheduled pixel position).  At scene time we load the chosen
;; variant's strip into `s0` and have guitarHero scroll a single
;; layer past the bar.  See `render-section-strip!` for the strip
;; layout and `guitarHero` (in the JS header) for the scroll math.
;;
;; `variants` is a list of per-variant tuples
;;   (list variant-key rhythm-url uniform-url cycle onsets)
;; where `variant-key` is the string the JS caller passes as
;; `opts.variant` to pick this variant.  `cycle` and `onsets` may
;; differ across variants (each one carries its own timeline), so
;; we bake them per-variant into the JS map.  Default variant is
;; the FIRST tuple in the list; the caller switches with
;; `<sid>_dance({ variant: 'b' })` etc.  When the source has no
;; `name@` tagging at all the realizer emits a single variant
;; keyed `'_default'`, so `<sid>_dance()` with no `variant` opt
;; just works exactly like the pre-variant world.
(define-for-syntax (dance-call-js sid variants)
  (define entries
    (for/list ([v (in-list variants)])
      (match-define (list key rhythm-url uniform-url cycle onsets) v)
      (define onsets-js
        (string-join
         (for/list ([o (in-list (append onsets (list cycle)))])
           (num->js o))
         ", "))
      (sa "    '" key "': {\n"
          "      rhythm:  '" rhythm-url "',\n"
          "      uniform: '" uniform-url "',\n"
          "      cycle:   " (num->js cycle) ",\n"
          "      onsets:  [" onsets-js "],\n"
          "    }")))
  (define default-key
    (cond [(null? variants) "_default"]
          [else (car (car variants))]))
  (sa "const " sid "_dance = (opts) => {\n"
      "  opts = opts || {};\n"
      "  const _mode = opts.mode || 'rhythm';\n"
      "  const _variants = {\n"
      (string-join entries ",\n") "\n"
      "  };\n"
      "  const _variant = (opts.variant != null) ? ('' + opts.variant) : '" default-key "';\n"
      "  const _data = _variants[_variant] || _variants['" default-key "'];\n"
      "  s0.initImage(_mode === 'uniform' ? _data.uniform : _data.rhythm);\n"
      "  return guitarHero(Object.assign({\n"
      "    cycle: _data.cycle,\n"
      "    onsets: _data.onsets,\n"
      "  }, opts));\n"
      "};"))

;; --- per-section scene rendering -------------------------------
;;
;; A section's `_scene()` is a one-shot "switch to me" action:
;;
;;   1. mute every other section's gain knob (so the previously
;;      active section's music stops) and unmute our own
;;   2. set the page body background to the section's `bg` color
;;      (resolved with inheritance: closest declared ancestor wins
;;      — same rule as the scribble realizer)
;;   3. load the section's image into `s0` and pipe it to `o0`,
;;      so calling `_scene()` swaps both the visual and the song
;;
;; Strudel's local dev server (under ~/git/strudel/website) serves
;; static files out of `website/public/`, so a PNG dropped at
;; `website/public/img/foo.png` is reachable from running patterns
;; as `/img/foo.png`.  We keep just the basename here and bolt on
;; the `/img/` prefix; the score author can drop the image in
;; `website/public/img/` (alongside `/dudes`) and everything
;; resolves automatically.
(define-for-syntax STRUDEL-IMAGE-PREFIX "/img/")

;; NOTE on quoting: Strudel's transpiler rewrites every
;; double-quoted string into a mini-notation pattern (so `"/img/x.png"`
;; becomes a parse error because of the `/`).  Single-quoted
;; strings are passed through untouched.  Always use single quotes
;; for image paths and any other plain JS string.

;; Resolve a possibly-relative image path against the directory
;; of the realize call's source file (same rule as the scribble
;; realizer uses), so the realizer can read PNG metadata
;; regardless of where the driver script is launched from.
(define-for-syntax (resolve-image-path raw)
  (cond [(absolute-path? raw) raw]
        [(current-load-relative-directory)
         (path->string
          (simplify-path
           (build-path (current-load-relative-directory) raw)))]
        [else raw]))

;; Try to read a PNG's pixel dimensions.  We need them at
;; realize-time to compute the Hydra `.scale()` letterbox factors
;; so images render with their natural aspect ratio instead of
;; stretching to fill the canvas.  If the file can't be opened we
;; silently fall back to a square aspect (1.0).
(define-for-syntax (png-aspect raw-path)
  (with-handlers ([exn:fail? (λ _ #f)])
    (define bm (read-bitmap (resolve-image-path raw-path)))
    (define w (send bm get-width))
    (define h (send bm get-height))
    (and (positive? w) (positive? h)
         (/ (exact->inexact w) (exact->inexact h)))))

;; Copy a section's image PNG into strudel's `website/public/img/`
;; so the URL we emit (`/img/<basename>`) actually resolves in the
;; browser.  Without this the author has to remember to copy the
;; file manually every time it changes (and we silently render a
;; broken-image background).  We overwrite unconditionally so any
;; tweak to the source PNG (e.g. regenerating brick-wall.png with
;; a new brick size) propagates on the next realize.
(define-for-syntax (publish-image-to-strudel! raw-path)
  (define src (resolve-image-path raw-path))
  (define dest-dir
    (path->string
     (expand-user-path
      (build-path "~" "git" "strudel" "website" "public" "img"))))
  (define dest (build-path dest-dir (file-name-from-path src)))
  (with-handlers ([exn:fail? (λ (e)
                               (eprintf ";; warn: couldn't publish ~a to strudel: ~a\n"
                                        src (exn-message e)))])
    (make-directory* dest-dir)
    (copy-file src dest #t)))

;; Build the JS body for one section's `_scene()` function.
;;   sid          : js id ("look_mom_twinkle")
;;   bg-color     : css color string (or #f)
;;   image-paths  : list of (image "path") src strings for the section
;;   all-gain-ids : every other section's "<sid>_g" let-binding name
;;   dance-info   : #f, or (cons url aspect) for a dance scene; when
;;                  set, the scene previews the dance's first pose
;;                  via the same letterbox pipeline as the static
;;                  image — the dance pattern itself then re-init's
;;                  s0 every beat to animate the pose.
;;
;; CONTAIN math (shared between static-image and dance scenes):
;;   _ia = image aspect (W/H), baked in at realize time
;;   _ca = canvas aspect, sampled from the window at scene call
;;   _sx = min(1, _ia/_ca)   ; <1 → letterbox bars on x
;;   _sy = min(1, _ca/_ia)   ; <1 → letterbox bars on y
;; `rect(_sx, _sy)` (defined in the file header) is white inside a
;; centered axis-aligned rectangle of width _sx and height _sy in
;; canvas units and black elsewhere.  Using it as .mask() keeps the
;; image fully visible inside its scaled rectangle and transparent
;; outside, and .layer()'ing the masked image onto a canvas-sized
;; `solid(…)` of the section's bg color gives us proper letterbox
;; bars.  (Hydra's built-in `shape(4)` is a diamond, not a
;; rectangle — using it as a mask snipped the corners off wide /
;; short images like stars-row.)
(define-for-syntax (letterboxed-image-js url aspect bg-color)
  (define bg-letterbox
    (cond [bg-color
           (format "  const [_br, _bg, _bb] = _cssToRGB('~a');\n" bg-color)]
          [else "  const [_br, _bg, _bb] = [0, 0, 0];\n"]))
  (string-append
   "  s0.initImage('" url "');\n"
   (format "  const _ia = ~a;\n" (real->decimal-string aspect 4))
   "  const _ca = (window.innerWidth || 1920) / (window.innerHeight || 1080);\n"
   "  const _sx = Math.min(1, _ia / _ca);\n"
   "  const _sy = Math.min(1, _ca / _ia);\n"
   bg-letterbox
   "  solid(_br, _bg, _bb)\n"
   "    .layer(src(s0).scale(1, _sx, _sy)\n"
   "                  .mask(rect(_sx, _sy)))\n"
   "    .out(o0);"))

(define-for-syntax (scene-body sid bg-color image-paths all-gain-ids has-dance?)
  (define mute-lines
    (string-join
     (for/list ([gid (in-list all-gain-ids)])
       (format "  ~a = ~a;"
               gid
               (if (string=? gid (string-append sid "_g")) "1" "0")))
     "\n"))
  (define bg-line
    (cond [bg-color
           (format "  document.body.style.background = '~a';" bg-color)]
          [else ""]))
  (define img-line
    (cond [has-dance?
           ;; Dance section: pure hydra.  Three priorities for the
           ;; scrolling background, in order:
           ;;   1. an explicit (image …) — load it into s1 (s0 is
           ;;      reserved for the dance strip texture) and pass
           ;;      `src(s1)` as the dance's `bg`.  The image fills
           ;;      the canvas behind the parade.
           ;;   2. otherwise, the section's `bg` color as a solid.
           ;;   3. otherwise, leave `bg` defaulted (black).
           (define bg-arg
             (cond [(not (null? image-paths))
                    (define src-path (car image-paths))
                    (define base (path->string (file-name-from-path src-path)))
                    (define url  (string-append STRUDEL-IMAGE-PREFIX base))
                    (sa "{ bg: src(s1) }")]
                   [bg-color
                    (sa "{ bg: solid(..._cssToRGB('" bg-color "')) }")]
                   [else "{}"]))
           (define s1-init
             (cond [(not (null? image-paths))
                    (define src-path (car image-paths))
                    (define base (path->string (file-name-from-path src-path)))
                    (define url  (string-append STRUDEL-IMAGE-PREFIX base))
                    (sa "  s1.initImage('" url "');\n")]
                   [else ""]))
           (sa s1-init "  " sid "_dance(" bg-arg ").out(o0);")]
          [(null? image-paths)
           "  solid(0, 0, 0).out(o0);"]
          [else
           (define src-path (car image-paths))
           (define base (path->string (file-name-from-path src-path)))
           (define url (string-append STRUDEL-IMAGE-PREFIX base))
           (define aspect (or (png-aspect src-path) 1.0))
           (letterboxed-image-js url aspect bg-color)]))
  (string-join
   (filter (λ (s) (not (string=? s "")))
           (list mute-lines bg-line img-line))
   "\n"))

;; --- the realizer itself ---------------------------------------

(define-art-realizer program-strudel-realizer
  (λ (stx)
    ;; Optional kwargs:
    ;;   #:dance-dir         disk dir to write pose PNGs into (default:
    ;;                       ~/git/strudel/website/public/img/dance/, so
    ;;                       the strudel local dev server serves them
    ;;                       as `/img/dance/arm-*.png` out of the box).
    ;;   #:dance-url-prefix  url prefix for the same files in the
    ;;                       generated strudel pattern (default:
    ;;                       /img/dance/).
    ;; The defaults match the existing scene-image convention but live
    ;; in their own `dance/` subdir so a hand-dropped section image
    ;; can't accidentally collide with a generated pose PNG.
    (define-values (dance-dir-val dance-url-prefix-val)
      (syntax-parse stx
        [(_ (~alt (~optional (~seq #:dance-dir dd:str)
                             #:defaults ([dd #'#f]))
                  (~optional (~seq #:dance-url-prefix dup:str)
                             #:defaults ([dup #'"/img/dance/"])))
            ...)
         (values
          (or (syntax-e #'dd)
              (path->string
               (expand-user-path
                (build-path "~" "git" "strudel" "website" "public" "img" "dance"))))
          (syntax-e #'dup))]))

    ;; Walk current-ctxt and bucket the exprs we care about by
    ;; section-id, preserving authoring order.
    (struct section-bucket [path id music scripts titles images] #:transparent)
    (define empty-bucket (section-bucket '() "_main" '() '() '() '()))

    (define section-order '())
    (define section-table (make-hash))

    (define (bucket-for! sid path)
      (cond [(hash-ref section-table sid #f)]
            [else
             (define b (section-bucket path sid '() '() '() '()))
             (set! section-order (cons sid section-order))
             (hash-set! section-table sid b)
             b]))

    (define (add! sid path field-getter field-setter expr)
      (define b (bucket-for! sid path))
      (define new
        (section-bucket
         (section-bucket-path b)
         (section-bucket-id b)
         (if (eq? field-getter section-bucket-music)
             (append (section-bucket-music b) (list expr))
             (section-bucket-music b))
         (if (eq? field-getter section-bucket-scripts)
             (append (section-bucket-scripts b) (list expr))
             (section-bucket-scripts b))
         (if (eq? field-getter section-bucket-titles)
             (append (section-bucket-titles b) (list expr))
             (section-bucket-titles b))
         (if (eq? field-getter section-bucket-images)
             (append (section-bucket-images b) (list expr))
             (section-bucket-images b))))
      (hash-set! section-table sid new))

    ;; section-path → bg color string.  Populated from `(bg "color")`
    ;; declarations at any depth.  Lookup uses ancestor-walk
    ;; (`bg-for-path` below), so a section inherits its closest
    ;; ancestor's bg unless it declares its own — matching the
    ;; scribble realizer's rule.
    (define path->bg (make-hash))

    (for ([e (current-ctxt)])
      (define path (section-path e))
      (define sid  (section-path->id path))
      (syntax-parse e
        [({~literal music} _ ...)
         (add! sid path section-bucket-music #f e)]
        [({~literal script} _ ...)
         (add! sid path section-bucket-scripts #f e)]
        [({~literal art-title} _:str)
         (add! sid path section-bucket-titles #f e)]
        [({~literal image} _:str)
         (add! sid path section-bucket-images #f e)]
        [({~literal bg} c:str)
         (hash-set! path->bg path (syntax-e #'c))]
        [_ (void)]))

    (set! section-order (reverse section-order))

    ;; Walk up `path` looking for the closest ancestor with a
    ;; declared `bg`; returns the color string or #f.
    (define (bg-for-path path)
      (let loop ([p path])
        (cond [(hash-ref path->bg p #f) => values]
              [(null? p) #f]
              [else (loop (drop-right p 1))])))

    ;; Names of every section's gain knob — each `_scene()` will
    ;; mute these all then unmute its own.
    (define all-gain-ids
      (for/list ([sid (in-list section-order)])
        (string-append sid "_g")))

    ;; ---- assemble the strudel program ------------------------

    (define header
      @sa|{
// =====================================================================
// auto-generated by program-strudel-realizer
//
// each art-section in the source program is rendered as:
//   - a block of `const <section>_<voice> = stack(...).slow(N)`
//   - a live-edit gain knob `let <section>_g = 1`
//   - a `$:` play line that consults that knob
//   - a parameterless `<section>_scene = () => ...` hydra recipe
//
// edit any gain knob to mute (0) / solo a section live; call any
// `<section>_scene()` to swap the visual.
// =====================================================================

// Hydra runs the visuals for every `<section>_scene()`; spin it
// up once at the top of the program so the very first scene call
// has something to draw into.  setcpm(120) likewise establishes
// a sane default tempo before any pattern is registered.
await initHydra();
setcpm(120);

samples({
  'sax':  'sax/000_notes121a.wav',
  'moog': { 'g3': 'moog/005_Mighty%20Moog%20G3.wav' },
}, 'github:tidalcycles/dirt-samples');

register('humanize', (amt, pat) => {
  const amtC = clamp(amt, 0, 1);
  return pat.withHaps((haps) => haps.map((hap) => {
    const offset = 0.1 * amtC * (2 * Math.random() - 1);
    return hap.withSpan((span) => span.withTime(t => t + offset));
  })).withValue((v) => ({
    ...v,
    velocity: (v.velocity ?? 1) + 0.5 * amtC * (2 * Math.random() - 1)
  }));
});

// CSS color name → [r, g, b] in 0..1.  Used by each section's
// hydra scene to paint the letterbox area around its image in
// the section's bg color.
window._cssToRGB = (name) => {
  const d = document.createElement('div');
  d.style.color = name;
  document.body.appendChild(d);
  const m = getComputedStyle(d).color.match(/\d+(\.\d+)?/g);
  d.remove();
  return m
    ? [parseInt(m[0]) / 255, parseInt(m[1]) / 255, parseInt(m[2]) / 255]
    : [0, 0, 0];
};

// Axis-aligned rectangle source for hydra: white inside a
// centered rect of width `w` and height `h` (in canvas units,
// 0..1), black elsewhere.  We need this instead of `shape(4, …)`
// — which is actually a diamond — so that thin / wide letterbox
// masks don't snip the corners off the underlying image.
//
// Note: strudel's transpiler treats backtick-template strings as
// mini-notation, so the glsl body is a single-quoted string (one
// line) — single-quoted strings are passed through untouched.
setFunction({
  name: 'rect',
  type: 'src',
  inputs: [
    { type: 'float', name: 'w', default: 1 },
    { type: 'float', name: 'h', default: 1 },
  ],
  glsl: 'vec2 _d = abs(_st - 0.5) - vec2(w, h) * 0.5; return vec4(vec3(step(max(_d.x, _d.y), 0.0)), 1.0);',
});

// scrollXClamp — like hydra's built-in scrollX, but clamps the
// resulting uv to [0, 1] instead of wrapping (mod 1).  Crucial for
// the guitarHero parade: when we shrink each pose with `.scale(<1)`
// or shift it past the canvas edge with `.scrollX(...)`, hydra's
// default GL_REPEAT sampling tiles the pose across the entire
// canvas; clamping the uv instead samples the source's EDGE pixels
// for everything outside [0,1], which (since our sprite cells have
// transparent borders) renders as transparent — exactly the "image
// is just gone here" behaviour we want for off-screen poses.
setFunction({
  name: 'scrollXClamp',
  type: 'coord',
  inputs: [
    { type: 'float', name: 'offsetX', default: 0 },
    { type: 'float', name: 'speed',   default: 0 },
  ],
  glsl: 'return clamp(_st + vec2(offsetX + time * speed, 0.0), 0.0, 1.0);',
});

// guitarHero — Hydra scrolling-pose display, like Guitar Hero.
//
// Single-layer pipeline: caller pre-renders a "playback strip" PNG
// for the section (see `render-section-strip!` in
// program-strudel.rkt) and loads the appropriate variant into s0
// based on `opts.mode`.  We build one hydra graph:
//
//     bg
//       .layer(src(s0).scrollX(...).scale(1, stripCells/vis, cellHeight)
//                                  .mask(rect(1, cellHeight)))
//       .layer(bar)
//
// — one src(s0), one .layer (plus the bar), regardless of frame
// count.  (Previously we layered one src(s0) per frame, which
// blew up the GLSL shader for long dances.)
//
// TWO MODES:
//   'rhythm'  (default — paired with the rhythm-mode strip, which
//             paints poses at their actual onset position):
//             Constant scroll rate.  The strip is `cycle` POSE-SIZE
//             cells wide; we just slide it so canvas X = barX
//             samples source uv.x = phase/cycle.  Fast rhythms
//             pack closely together visually; slow rhythms leave
//             empty space between poses.  This is the cadence-
//             faithful Guitar Hero feel — what the chart "looks
//             like" matches what the rhythm IS.
//
//   'uniform' (paired with the uniform-mode strip, one cell per
//             frame at uniform spacing):
//             Variable scroll rate.  We interpolate the strip's
//             scroll position PIECEWISE-LINEARLY through `onsets`,
//             so pose i is centered on the bar at phase = onsets[i]
//             exactly; over [onsets[i], onsets[i+1]] the scroll
//             advances ONE cell (taking longer for big rhythms,
//             shorter for fast rhythms).  All poses get the same
//             on-screen footprint; the music's rhythm shows up
//             purely as varying scroll speed.
//
//   opts = {
//     mode:       'rhythm' (default) or 'uniform' — see above.
//     cycle:      REQUIRED — total dance length in beats; phase
//                 wraps modulo this.
//     onsets:     REQUIRED — array of length N+1: the start beat
//                 of each frame followed by `cycle` as the closing
//                 endpoint.  Used to position the strip's cell
//                 widths in 'rhythm' mode and to interpolate the
//                 scroll in 'uniform' mode.
//     vis:        number of poses (or beats — see mode) visible
//                 across the canvas width (default 4).
//     cellHeight: canvas-units tall to render each pose (default
//                 0.5 — half canvas).  Independent of `vis`; the
//                 strip is stretched anisotropically.
//     barX:       SCROLL ANCHOR position in canvas coords 0..1
//                 (default 0.2).  This is the canvas-x where
//                 frame N's center lands at phase N — i.e.
//                 where the dudes "play" — and so it determines
//                 the dudes' absolute canvas positions over
//                 time.  Changing this slides the entire
//                 scrolling plot (both bar and dudes) together,
//                 since the dudes are aligned RELATIVE to the
//                 anchor.  To move just the visible yellow line
//                 without shifting the dudes, use `barDrawX`
//                 below.  Larger barX = more "history" room
//                 left of the anchor; smaller = longer
//                 anticipation runway right of it.
//     barDrawX:   where to actually DRAW the yellow bar line
//                 (canvas coords 0..1, defaults to `barX`).
//                 The yellow line is purely decorative — moving
//                 it doesn't reflow any of the dance timing or
//                 dude positions, it just relocates the visible
//                 indicator.  Useful when you like where the
//                 dudes are landing but want the line nudged
//                 over for visual taste.
//     bg:         a hydra source for the background (default
//                 solid(0, 0, 0)).
//     hold:       number of cycles to wait before the dance is
//                 "due" at the bar (default 0).  The strip
//                 still scrolls continuously underneath — but
//                 a reveal mask, anchored at canvas right and
//                 following frame 0's canvas position leftward,
//                 hides the parade until frame 0 reaches the
//                 bar at strudel-time `hold`.  The caller is
//                 responsible for making the music start at
//                 strudel-time `hold` too (e.g. via `.late(hold)`
//                 on the music pattern); otherwise the dance
//                 and music will be misaligned.  Negative
//                 values are clamped to 0.
//     phaseOffset: number of beats to shift the dance phase by
//                 (default 0).  Useful for nudging the dance
//                 left/right if there's a one-cell off-by-one
//                 you want to dial out manually.  Positive
//                 shifts the parade so a LATER frame lands on
//                 the bar at phase 0; negative the opposite.
//     revealPad:  extra canvas units to push the reveal's left
//                 edge LEFT of frame 0's center (default
//                 `1.5/vis`).  Dudes are `1/vis` canvas wide
//                 and spaced `1/vis` apart, so the safe (non-
//                 chopping) curtain positions sit between
//                 dudes — at offsets of `0.5/vis`, `1.5/vis`,
//                 `2.5/vis`, ...  ANY other value puts the
//                 curtain through the middle of some dude, who
//                 then appears bisected.  The default
//                 `1.5/vis` lands the curtain at frame -1's
//                 left edge (= cleanly between frame -2 and
//                 frame -1, the trailing wrap-around dudes from
//                 the previous loop), revealing frame 0 AND
//                 the most-recent wrap dude in full.  Use
//                 `0.5/vis` for a tighter reveal that hides all
//                 wrap dudes; `2.5/vis` to expose another wrap
//                 dude.  Only affects scenes with `hold > 0`.
//     timeFn:     () => time in beats; default `getTime()`,
//                 strudel's scheduler position (already in cycles,
//                 so 1 cycle = 1 beat once the music is
//                 .slow(totalBeats)-locked).
//   }
//
// Returns a hydra render node — caller is responsible for `.out()`.
//
// Geometry derivation (hydra's `scale(a, x, y)` maps a SOURCE uv
// span of `w` to a CANVAS uv span of `w * (a * axis-mult)` — i.e.
// the scale factor is the canvas-units-per-source-unit ratio):
//   * Horizontal: a 1-cell span on the strip is 1/stripCells of
//     source-x.  We want that to appear as 1/vis canvas wide, so
//     (1/stripCells) × scale_x = 1/vis ⇒ scale_x = stripCells/vis.
//     stripCells = cycle in 'rhythm' mode (one cell per beat),
//                = N      in 'uniform' mode (one cell per frame).
//   * Vertical:   the strip's full source-y span 1 should map to
//     `cellHeight` on canvas, so scale_y = cellHeight.  Easy sign
//     error: scale_y = 1/cellHeight instead shrinks the visible
//     source-y to a thin band and GL_REPEATs the strip vertically.
//
// Scroll: hydra's chain `src(s0).scrollX(s).scale(1, sx, sy)` first
// remaps canvas uv → source uv via scale, then offsets source.x
// by `s`.  We want canvas X = barX to sample the per-phase target
// source uv.x:
//   s = targetSrcX - 0.5 - (barX - 0.5)/scale_x
//
// Using plain scrollX (not scrollXClamp) lets hydra wrap the strip
// in x via GL_REPEAT, so the END of the strip naturally appears to
// the LEFT of the start as `phase` crosses `cycle` — seamless loop.
//
// The strip ALSO wraps vertically because scale_y < 1 pushes
// source-y outside [0,1] for any canvas y outside the central
// cellHeight-tall band.  We mask with a centered `rect(1,
// cellHeight)` so only that band is opaque; the bg shows through
// above and below.
window.guitarHero = function guitarHero(opts) {
  opts = opts || {};
  const mode       = opts.mode        || 'rhythm';
  const cycle      = (opts.cycle      != null) ? opts.cycle      : 1;
  const onsets     = Array.isArray(opts.onsets) ? opts.onsets    : [0, cycle];
  const vis        = (opts.vis        != null) ? opts.vis        : 4;
  const cellHeight = (opts.cellHeight != null) ? opts.cellHeight : 0.5;
  const barX       = (opts.barX       != null) ? opts.barX       : 0.2;
  const barDrawX   = (opts.barDrawX   != null) ? opts.barDrawX   : barX;
  const bg         = opts.bg          || solid(0, 0, 0);
  const hold        = Math.max(0, (opts.hold    != null) ? opts.hold    : 0);
  const phaseOffset = (opts.phaseOffset != null) ? opts.phaseOffset : 0;
  const revealPad   = (opts.revealPad != null) ? opts.revealPad : 1.5 / vis;
  const timeFn = opts.timeFn || (() => {
    try { if (typeof getTime === 'function') { const v = getTime(); if (isFinite(v)) return v; } } catch (e) {}
    return 0;
  });

  const N = Math.max(1, onsets.length - 1);                  // frame count
  const stripCells = (mode === 'uniform') ? N : cycle;
  const scaleX = stripCells / vis;
  const scaleY = cellHeight;
  const barOffset = (barX - 0.5) / scaleX;                   // precomputed

  // The bar is a thin vertical yellow line at canvas-x =
  // barDrawX.  This is purely visual — the scroll math anchors
  // off `barX` (the SCROLL ANCHOR), which determines where
  // dudes actually land at note time.  shape(4).scale(10, 0.002)
  // is a tall narrow rectangle; scrollX shifts its source so the
  // white strip lands at canvas-x = barDrawX (source 0.5 → canvas
  // 0.5 − scrollX, so scrollX = 0.5 − barDrawX).
  const bar = solid(1, 1, 0)
    .mask(shape(4).scale(10, 0.002).scrollX(0.5 - barDrawX));

  // Pick a phase → targetSrcX function based on mode.  Both produce
  // a source uv.x in roughly [0, 1] (with floats outside that range
  // wrapping cleanly via GL_REPEAT on the strip).
  const targetAtPhase = (mode === 'uniform')
    ? ((phase) => {
        // Find interval [onsets[i], onsets[i+1]] containing phase.
        // Linear scan — N is typically O(10s).
        let i = 0;
        for (; i < N - 1; i++) {
          if (phase < onsets[i + 1]) break;
        }
        const intervalDur = onsets[i + 1] - onsets[i];
        const progress = intervalDur > 0
          ? (phase - onsets[i]) / intervalDur
          : 0;
        // Pose i sits at cell center (i+0.5)/N at progress=0 and
        // slides to (i+1.5)/N at progress=1.
        return (i + 0.5 + progress) / N;
      })
    : ((phase) => phase / cycle);                            // 'rhythm'

  return bg
    .layer(
      src(s0)
        .scrollX(() => {
          let raw = timeFn() - hold + phaseOffset;
          if (!isFinite(raw)) raw = 0;
          // Frame 0 lands on the bar at strudel-time `hold`
          // (and every `cycle` strudel-time units after).
          // `phaseOffset` nudges the whole parade left/right
          // in beat-units if you need to dial out a one-cell
          // off-by-one.
          const phase = ((raw % cycle) + cycle) % cycle;
          return targetAtPhase(phase) - 0.5 - barOffset;
        }, 0)
        .scale(1, scaleX, scaleY)
        // Reveal mask: a rect cellHeight tall whose LEFT edge
        // tracks the leading dude (frame 0)'s canvas position,
        // offset leftward by `revealPad` so the dude is fully
        // inside the reveal (not bisected by the curtain edge).
        // Frame 0 lives at canvas-x = barX exactly at
        // strudel-time `hold`; before then it sits (hold − t)/vis
        // canvas units to its right (off-screen for sufficiently
        // early t).  The mask spans [leftEdge, 1] on canvas —
        // right edge pinned at canvas right — so the curtain
        // literally follows the dude as he walks in.
        //
        // After frame 0 walks past canvas-x = 0 the dude formula
        // would go negative; clamp `leftEdge` to [0, 1] so the
        // mask locks at fully open (width 1, centered, same as
        // `rect(1, cellHeight)`) for the rest of the run.
        //
        // scrollX `s` shifts source.x by `s` before the rect
        // shader samples it, so the rect (centered at source 0.5)
        // appears centered at canvas-x = 0.5 − s.  A right-
        // anchored rect of width w has canvas center at 1 − w/2
        // = (1 + leftEdge)/2, hence s = 0.5 − (1 + leftEdge)/2
        // = −leftEdge/2.
        .mask(rect(
          () => {
            const t = timeFn();
            if (!isFinite(t)) return 0;
            const f0x = barX + (hold - t) / vis;
            const leftEdge = Math.max(0, Math.min(1, f0x - revealPad));
            return 1 - leftEdge;
          },
          cellHeight
        ).scrollX(() => {
          const t = timeFn();
          if (!isFinite(t)) return 0;
          const f0x = barX + (hold - t) / vis;
          const leftEdge = Math.max(0, Math.min(1, f0x - revealPad));
          return -leftEdge / 2;
        }))
    )
    .layer(bar);
};

}|)

    ;; Per-section, per-variant dance frames (pose timeline).
    ;; sid → hash: variant-sym-or-#f → (cons frames total-len).
    ;; An empty hash means the section has no dance at all.  All
    ;; variants within a section share the same total-len (the
    ;; music timeline) but can carry differing frames/onsets.
    (define dance-table (make-hash))
    (for ([sid (in-list section-order)])
      (define b (hash-ref section-table sid))
      (define v->ft (blocks->dance-frames (section-bucket-music b)))
      (hash-set! dance-table sid v->ft))

    ;; For each section: the unique pose tags it uses across ALL
    ;; variants.  Used below to (a) gate dance-rendering work on
    ;; the section actually having a dance, and (b) deduplicate
    ;; per-pose PNG rendering — two variants that share a pose
    ;; only render its PNG once.
    (define section-pose-tags
      (for/hash ([sid (in-list section-order)])
        (define v->ft (hash-ref dance-table sid))
        (define seen (mutable-set))
        (define unique '())
        (for ([(_ ft) (in-hash v->ft)])
          (for ([f (in-list (car ft))])
            (define tag (list (caddr f) (cadddr f) (list-ref f 4)))
            (unless (set-member? seen tag)
              (set-add! seen tag)
              (set! unique (cons tag unique)))))
        (values sid (reverse unique))))

    ;; Render one PLAYBACK STRIP PNG per section: a horizontal canvas
    ;; of (cycle × POSE-SIZE) × POSE-SIZE pixels, with each frame's
    ;; pose painted centered at its scheduled-time pixel position.
    ;; Hydra loads this single image and scrolls it past the bar at
    ;; scene time, so it's the only image the dance pipeline actually
    ;; loads.  Per-pose `arm-<l>-<r>-<face>.png` files are *also*
    ;; written next to the strip (deduped across sections — identical
    ;; poses shared between sections render once) so they're available
    ;; outside guitarHero too (e.g. handy in ad-hoc hydra patches that
    ;; just want a single still pose).
    (define any-dance?
      (for/or ([sid (in-list section-order)])
        (not (null? (hash-ref section-pose-tags sid)))))
    (when any-dance?
      (make-directory* dance-dir-val))
    (define all-unique-poses
      (for/fold ([acc (set)]) ([sid (in-list section-order)])
        (for/fold ([acc acc]) ([t (in-list (hash-ref section-pose-tags sid))])
          (set-add acc t))))
    (for ([p (in-set all-unique-poses)])
      (match-define (list l r face) p)
      (define filename (format "arm-~a.png" (pose-tag l r face)))
      (render-dance-png! l r face (build-path dance-dir-val filename)))
    ;; sid → list of per-variant tuples
    ;;   (list variant-key rhythm-url uniform-url cycle-in-beats onsets-list)
    ;; in stable order — `#f` (unnamed) first, then named variants
    ;; alphabetically — so the JS wrapper's default-variant pick
    ;; doesn't bounce around between realizes.  An empty list
    ;; means the section has no dance.  We have to redo the
    ;; divisions-rescale that section-blocks does for the .slow()
    ;; math (raw musicxml units → beats) here too, so the JS-side
    ;; onsets array agrees with the music's `.slow(total/divisions)`
    ;; and pose i lands on the bar exactly when the music's
    ;; playhead is at beat onsets[i].
    ;;
    ;; For each variant, we render BOTH strip layouts (rhythm +
    ;; uniform); the JS-side `<sid>_dance` wrapper picks which to
    ;; load based on `opts.mode` at scene-call time, and which
    ;; VARIANT to load based on `opts.variant`.  Both layouts are
    ;; cheap (one PNG each), and shipping both means the user can
    ;; flip between modes (and variants) live in the REPL without
    ;; re-running the realizer.
    ;;
    ;; PNG naming: unnamed variant keeps the pre-multivariant
    ;; filenames `arm-strip-<sid>.png` so a single-variant score
    ;; doesn't churn its emitted PNGs; named variants append
    ;; `-<variant>` to disambiguate.
    (define (variant-sort-key v)
      ;; #f (unnamed) sorts first, then symbols alphabetically.
      (cond [(not v) ""]
            [else (string-append "_" (symbol->string v))]))
    (define section-strip
      (for/hash ([sid (in-list section-order)])
        (define b (hash-ref section-table sid))
        (define v->ft (hash-ref dance-table sid))
        (define divisions (blocks->divisions (section-bucket-music b)))
        (define variant-keys
          (sort (hash-keys v->ft)
                string<?
                #:key variant-sort-key))
        (define tuples
          (for/list ([v (in-list variant-keys)])
            (define raw-frames (car (hash-ref v->ft v)))
            (cond
              [(null? raw-frames) #f]
              [else
               (define frames
                 (for/list ([f (in-list raw-frames)])
                   (define start (/ (car f) divisions))
                   (define dur   (/ (cadr f) divisions))
                   (list start dur (caddr f) (cadddr f) (list-ref f 4))))
               (define cycle
                 (apply max (for/list ([f (in-list frames)])
                              (+ (car f) (cadr f)))))
               (define onsets (map car frames))
               (define suffix (if v (format "-~a" v) ""))
               (define rhythm-name  (format "arm-strip-~a~a.png" sid suffix))
               (define uniform-name (format "arm-strip-uniform-~a~a.png" sid suffix))
               (render-section-strip! frames cycle 'rhythm
                                      (build-path dance-dir-val rhythm-name))
               (render-section-strip! frames cycle 'uniform
                                      (build-path dance-dir-val uniform-name))
               (define key (if v (symbol->string v) "_default"))
               (list key
                     (string-append dance-url-prefix-val rhythm-name)
                     (string-append dance-url-prefix-val uniform-name)
                     cycle
                     onsets)])))
        (values sid (filter values tuples))))

    (define section-blocks
      (for/list ([sid (in-list section-order)])
        (define b (hash-ref section-table sid))
        (define path (section-bucket-path b))
        (define titles
          (for/list ([t (in-list (section-bucket-titles b))])
            (syntax-parse t [(_ s:str) (syntax-e #'s)])))
        (define scripts
          (for/list ([s (in-list (section-bucket-scripts b))])
            (script-comment s)))
        (define images
          (for/list ([im (in-list (section-bucket-images b))])
            (syntax-parse im [(_ src:str) (syntax-e #'src)])))
        ;; merge all music blocks in this section into per-voice
        ;; lists, concatenating blocks in time (so two
        ;; back-to-back `(music …)` exprs play sequentially, not
        ;; on top of each other — e.g. swallowtail-jig's A then B).
        (define-values (voice->tc-merged total-len)
          (blocks->voice->tc (section-bucket-music b)))

        ;; .slow(L) makes one pattern instance take L cycles; at
        ;; setcpm(N) a cycle is 60/N sec.  Our timecat weights are
        ;; raw musicxml duration units, with `divisions` units per
        ;; quarter note (e.g. divisions=2 ⇒ 1 unit = 1 eighth).  To
        ;; make 1 quarter note play in 60/N sec (= conventional BPM
        ;; of N), we need each unit to play in 60/(N*divisions) sec,
        ;; which means slowing by total-len/divisions cycles.  With
        ;; divisions=1 this is a no-op; with divisions=2 it halves
        ;; the slow value so the music plays twice as fast (each
        ;; unit being an eighth and getting an eighth's worth of
        ;; time).
        (define divisions (blocks->divisions (section-bucket-music b)))
        (define slow-cycles (/ total-len divisions))

        (define bg-color (bg-for-path path))
        (define image-srcs
          (for/list ([im (in-list (section-bucket-images b))])
            (syntax-parse im [(_ s:str) (syntax-e #'s)])))
        ;; Mirror each `(image …)` PNG into strudel's public/img/
        ;; alongside the dance strips, so the `/img/<basename>` URLs
        ;; that scene-body emits actually resolve in the browser.
        (for-each publish-image-to-strudel! image-srcs)

        ;; Dance for this section (precomputed above).  We emit a
        ;; `<sid>_dance()` constructor function that loads this
        ;; section's pre-rendered playback strip into s0 and returns
        ;; a hydra render node (a single scrolling layer past the
        ;; bar).  scene-body invokes it when section-has-dance? is
        ;; true.
        ;;
        ;; `section-strip` already rescaled the dance from raw
        ;; musicxml units to beats (divided by divisions), matching
        ;; the music's `.slow(total/divisions)` so dance and music
        ;; loop at the same rate.  The dance "cycle" is the
        ;; dance's own length (the strip's pixel width / POSE-SIZE),
        ;; not the music's length — that way a short dance riding
        ;; on top of a long piece keeps looping continuously
        ;; through the music instead of stopping and waiting for
        ;; the music to loop.  Override `cycle` in
        ;; `<sid>_dance({ cycle: … })` to taste.
        (define strip-info (hash-ref section-strip sid))
        (define section-has-dance? (and strip-info (cons? strip-info)))
        (define dance-js
          (cond [(not section-has-dance?) ""]
                [else (dance-call-js sid strip-info)]))
        ;; For the DANCE comment we report the FIRST variant's
        ;; cycle (matches the JS wrapper's default).  Variants
        ;; typically share a cycle since the music timeline is
        ;; the same, but if they don't this just picks the first.
        (define dance-len
          (cond [(not section-has-dance?) 0]
                [else (list-ref (car strip-info) 3)]))

        (define section-comment
          (apply string-append
                 (list
                  "// ─────────────────────────────────────────────────────\n"
                  (format "// section: ~a\n"
                          (if (null? path) "_main" (string-join (map ~a path) " > ")))
                  "// ─────────────────────────────────────────────────────\n"
                  (apply string-append
                         (for/list ([t (in-list titles)])
                           (format "// TITLE: ~a\n" t)))
                  (apply string-append
                         (for/list ([s (in-list scripts)])
                           (string-append "// SCRIPT:\n" s "\n")))
                  (apply string-append
                         (for/list ([im (in-list images)])
                           (format "// IMAGE: ~a\n" im)))
                  (if bg-color (format "// BG: ~a\n" bg-color) "")
                  (cond
                    [(not section-has-dance?) ""]
                    [else
                     (define v->ft (hash-ref dance-table sid))
                     (define lines
                       (for/list ([v (in-list (sort (hash-keys v->ft)
                                                    string<?
                                                    #:key (λ (x) (if x (symbol->string x) ""))))])
                         (define frames (car (hash-ref v->ft v)))
                         (define vlabel (if v (symbol->string v) "_default"))
                         (format "// DANCE[~a]: ~a frames over ~a beats\n"
                                 vlabel (length frames) (num->js dance-len))))
                     (apply string-append lines)]))))

        ;; per-voice consts + a stacking const for live $:.  We
        ;; tack `._punchcard()` onto each voice const so the strudel
        ;; REPL renders a per-voice punchcard underneath the
        ;; pattern once it's played — useful for eyeballing the
        ;; rhythm against the dance.  Underscore version (vs plain
        ;; `.punchcard()`) returns the pattern, keeping the value
        ;; chainable for the `$:` line below.
        (define voice-consts
          (string-join
           (for/list ([(v tcs) (in-hash voice->tc-merged)])
             @sa{const @|sid|_@|v| = stack(@(string-join tcs ",\n  ")).slow(@(num->js slow-cycles))._punchcard();})
           "\n"))

        ;; Gain knob starts muted (0); calling this section's
        ;; `_scene()` is what unmutes it.  Edit by hand to mix.
        (define gain-line @sa{let @|sid|_g = 0;})
        (define scene-line
          (string-append
           "const " sid "_scene = () => {\n"
           (scene-body sid bg-color image-srcs all-gain-ids section-has-dance?)
           "\n};"))

        (apply string-append
               (list section-comment
                     (if (string=? voice-consts "") "" (string-append voice-consts "\n"))
                     (if (string=? dance-js "") "" (string-append dance-js "\n"))
                     gain-line "\n"
                     scene-line "\n\n"))))

    ;; PLAY block — every section's `$:` line is live, but each
    ;; line's `.gain(<sid>_g)` reads from a let-binding initialized
    ;; to 0 above.  Calling a `_scene()` rewrites all the gains so
    ;; exactly one section plays.  The author can also edit any
    ;; gain by hand to layer / mix sections live.
    (define play-block
      (string-append
       "// ─────────────────────────────────────────────────────\n"
       "// PLAY — every section's $: is live; gains start at 0.\n"
       "// call `<section>_scene()` to switch to that section\n"
       "// (mutes everything else, sets bg+image, unmutes me).\n"
       "// edit any `*_g` directly to mix.\n"
       "// ─────────────────────────────────────────────────────\n"
       (apply string-append
              (for/list ([sid (in-list section-order)])
               (define b (hash-ref section-table sid))
               (define-values (v→tc _len)
                 (blocks->voice->tc (section-bucket-music b)))
               (cond
                 [(zero? (hash-count v→tc)) ""]
                 [else
                  (define music-args
                    (string-join (for/list ([v (in-hash-keys v→tc)])
                                   (format "~a_~a" sid v))
                                 ", "))
                  ;; The dance no longer needs a `$:` line — the
                  ;; guitarHero overlay reads `getTime()*cps()` and
                  ;; animates itself once `<sid>_scene()` is called.
                  (string-append @sa{$: stack(@music-args).gain(@|sid|_g)} "\n")])))))

    (define hydra-block
      (string-append
       "// ─────────────────────────────────────────────────────\n"
       "// SCENES — hydra is already initialized at the top of\n"
       "// the file; each `_scene()` below mutes other sections,\n"
       "// unmutes itself, sets the page background to its `bg`\n"
       "// color, and loads its png.  uncomment one to switch.\n"
       "// ─────────────────────────────────────────────────────\n"
       (apply string-append
              (for/list ([sid (in-list section-order)])
                (string-append @sa{// @|sid|_scene();} "\n")))))

    #`#,(string-append header
                       "\n"
                       (string-join section-blocks "\n")
                       "\n"
                       hydra-block
                       "\n"
                       play-block)))
