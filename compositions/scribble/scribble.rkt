#lang at-exp racket

;; A scribble realizer for tonart.
;;
;; Accepts a mix of `text` art objects and tonart's `music` embedding,
;; and macro-expands at the realize site into a sequence of
;; scribble/manual forms (title / para / image / centered).
;;
;; Each `music` block is rendered to a PNG via the lilypond realizer
;; + the `lilypond --png` shell command, then embedded back into the
;; scribble doc as `(centered (image "...png"))`.
;;
;; Typical use, inside a `#lang scribble/manual` source:
;;
;;   @(require tonart "scribble.rkt" "../lilypond/lilypond.rkt"
;;             other-stuff-that-defines-the-doc)
;;   @(realize (scribble-realizer #:title "My Doc") my-doc)
;;
;; my-doc is a `define-art` that uses `text` and `music`:
;;
;;   (define-art my-doc
;;     (text "Here is a scale:")
;;     (music
;;       (voice@ (s)
;;         (i@ [0 4]
;;           (seq (note c 0 4) (note d 0 4) (note e 0 4) (note f 0 4))
;;           (uniform-rhythm 1) (apply-rhythm))))
;;     (text "And again."))

(require (except-in tonart direction transpose-octave)
         ;; the demo wants a one-stop `(transpose-octave n)` rewriter
         ;; that bundles the four-step note->^o / declare / run / ^o->note
         ;; chain.  the upstream art-object also called `transpose-octave`
         ;; is still needed inside that chain to declare the intent,
         ;; so we import it here under a fresh name.
         (rename-in (only-in tonart transpose-octave)
                    [transpose-octave declare-transpose-octave])
         (except-in "../lilypond/lilypond.rkt" image)
         "../script/script.rkt"
         art/coordinate/name
         (only-in scribble/manual title section subsection subsubsection
                                  subsubsub*section
                                  para centered bold italic nested elem larger
                                  ;; scribble's `image` is renamed
                                  ;; locally so we can define our own
                                  ;; `image` art-object that the
                                  ;; realizer expands to a scribble
                                  ;; (centered (scr-image …)) call.
                                  [image scr-image])
         (only-in scribble/decode splice)
         (only-in scribble/core make-style)
         (only-in scribble/latex-properties tex-addition)
         (only-in scribble/html-properties attributes)
         racket/runtime-path
         (for-syntax syntax/parse racket/format racket/file racket/path
                     racket/system racket/string racket/class racket/draw
                     racket/list
                     (only-in art/private/core delete-expr remove-from-id-ctxt
                              set-id-ctxt realizer/s-body current-ctxt lookup-ctxt)
                     art/coordinate/index
                     art/coordinate/name))

;; `art-section` name-coordinate.  Each piece (and category of
;; pieces) in the porchfest programme is wrapped in
;; `(@ [(art-section "Piece Name")] ...)` so that an editor can
;; `(@ [(art-section "Piece Name")] (zoom))` to focus on it.  Like
;; all name coordinates, nesting two `(art-section ...)` coords
;; concatenates their paths — `(art-section "Look mom...")` wrapping
;; `(art-section "Row Row Row...")` gives items tagged
;; `(art-section "Look mom..." "Row Row Row...")`, which the realizer
;; reads as a two-level heading hierarchy.
(define-name-coordinate art-section)

;; lexicographic compare on index lists.  used by the realizer to
;; sort `(current-ctxt)` by the `index` coordinate (set up by an
;; `ix--` wrapping the score body).
(define-for-syntax (index<? a b)
  (cond [(and (null? a) (null? b)) #f]
        [(null? a) #t]
        [(null? b) #f]
        [(< (car a) (car b)) #t]
        [(> (car a) (car b)) #f]
        [else (index<? (cdr a) (cdr b))]))

;; runtime path to the compact-layout LaTeX add-on.  attached to the
;; document title's style as a `tex-addition` when #:compact? is true.
(define-runtime-path compact-tex "compact.tex")

;; two-column add-on: defines the `tonartcols` LaTeX environment that
;; the realizer wraps the body in when called with #:columns 2.
(define-runtime-path two-col-tex "two-col.tex")

;; heading-fix: override acmart's run-in \subsubsection / \paragraph
;; so art-subsection / art-subsubsection (which map to those LaTeX
;; commands when section-depth > 0) render as proper display
;; headings.  Wired into `heading-style-for` so it gets included in
;; the LaTeX preamble whenever the realizer emits any heading.
(define-runtime-path heading-fix-tex "heading-fix.tex")

;; bg.tex: shared preamble for per-section background colors in PDF
;; output (xcolor + tcolorbox setup).  Per-color environments
;; themselves are generated on the fly — see `bg-tex-preamble` and
;; `bg-color-env-def` below.
(define-runtime-path bg-tex "bg.tex")

;; redact.tex: defines `\TonartRedact{text}` for the redact art
;; object — a tight black bar the width/height of `text`.
(define-runtime-path redact-tex "redact.tex")

;; Inline element style for a `(redact "...")` art object: a black
;; bar of the same width and height as the contained text, in both
;; PDF (via the \TonartRedact macro) and HTML (via inline CSS that
;; paints the background black and the text transparent).  The
;; element's text content is intentionally preserved so it remains
;; selectable / searchable in the HTML output but invisible to
;; readers.
(define redact-style
  (make-style "TonartRedact"
              (list (tex-addition redact-tex)
                    (attributes
                     '((style . "background:#000;color:transparent;")))))) 

(define compact-style
  (make-style #f (list (tex-addition compact-tex))))

(define two-col-doc-style
  (make-style #f (list (tex-addition two-col-tex))))

;; Sentinel paragraph styles that emit a bare \begin{multicols}{2} /
;; \end{multicols} into the LaTeX output.  See two-col.tex for the
;; matching \tonartcolsbegin / \tonartcolsend macro definitions.  The
;; begin sentinel carries the `tex-addition` itself so the macros get
;; defined regardless of whether the title (which usually carries the
;; addition) is being emitted.
(define two-col-begin-style
  (make-style "tonartcolsbegin" (list (tex-addition two-col-tex))))
(define two-col-end-style   (make-style "tonartcolsend"   '()))

;; Heading style with the heading-fix tex-addition attached.  The
;; 'unnumbered behavior in scribble comes from having the 'unnumbered
;; symbol in the style's properties list (see convert-part-style in
;; scribble/private/tag.rkt), NOT from the style's name — so we add
;; it as a property here when numbered? is false.
(define (heading-style-for numbered?)
  (make-style #f
              (cons (tex-addition heading-fix-tex)
                    (if numbered? '() '(unnumbered)))))

;; A LaTeX environment name (`TonartBg<sanitized>`) is generated per
;; unique color used in a document.  We can't pass the color to a
;; single shared environment because scribble's LaTeX renderer
;; ignores `command-extras` on nested-flows — see
;; `do-render-nested-flow` in scribble/latex-render.rkt — so we bake
;; the color into the environment definition instead.
;;
;; `sanitize-color-name` produces a LaTeX-safe identifier suffix:
;; letters only, no `#` or punctuation (xcolor names allow letters
;; and digits, but `\newenvironment` names must use letters only).
;; Colors that sanitize to the same string would collide, so we tag
;; each with a counter (built in `build-bg-env-defs`).
(define-for-syntax (sanitize-color-name color)
  (define s (regexp-replace* #rx"[^A-Za-z]+" color ""))
  (if (= (string-length s) 0) "x" s))

;; Build a `\definecolor` line for `color` bound to `name`.  We
;; always emit HTML hex so we don't depend on which xcolor options
;; (svgnames, dvipsnames, …) the surrounding document has loaded:
;;   - "#rrggbb" or "rrggbb"  → emitted as-is
;;   - otherwise              → looked up in racket/draw's color
;;                              database, which knows the full CSS
;;                              named-color set ("yellow", "tan",
;;                              "lightblue", …); falls back to a
;;                              `\colorlet{...}{name}` so a custom
;;                              xcolor name still works if defined
;;                              elsewhere.
(define-for-syntax (color->hex color)
  (cond
    ;; #px (perl-style) is required for `{6}` quantifier; with the
    ;; POSIX #rx the bound-repetition silently doesn't match and
    ;; we used to fall through to `\colorlet{...}{#ffec73}`, which
    ;; xcolor can't parse.
    [(regexp-match #px"^#?([0-9a-fA-F]{6})$" color)
     => (λ (m) (cadr m))]
    [else
     (define c (send the-color-database find-color color))
     (and c (format "~a~a~a"
                    (~r (send c red)   #:base 16 #:min-width 2 #:pad-string "0")
                    (~r (send c green) #:base 16 #:min-width 2 #:pad-string "0")
                    (~r (send c blue)  #:base 16 #:min-width 2 #:pad-string "0")))]))

(define-for-syntax (color-def-line name color)
  (cond
    [(color->hex color) => (λ (hex) (format "\\definecolor{~a}{HTML}{~a}" name hex))]
    [else (format "\\colorlet{~a}{~a}" name color)]))

;; Build the LaTeX `\newenvironment` (plus needed `\definecolor` /
;; `\colorlet` lines) that bakes both `fg` and `bg` into a single
;; environment.  Either argument may be `#f`; missing colors are
;; simply omitted from the env body so we can have bg-only, fg-only,
;; or both-colored sections.  Returns two values: the env name
;; (used as the scribble style-name on the wrapping nested-flow)
;; and the LaTeX source string that defines the env.
(define-for-syntax (bg-color-env-def fg bg id)
  (define suffix
    (string-append (if fg (sanitize-color-name fg) "x")
                   (if bg (sanitize-color-name bg) "x")
                   (number->string id)))
  (define env-name (format "TonartBg~a" suffix))
  (define bg-color-name (and bg (format "tonartbg~a" suffix)))
  (define fg-color-name (and fg (format "tonartfg~a" suffix)))
  (define defs
    (string-append
     (if bg (string-append (color-def-line bg-color-name bg) "\n") "")
     (if fg (string-append (color-def-line fg-color-name fg) "\n") "")))
  (define tcb-opts
    (string-append
     (if bg
         (format "colback=~a,colframe=~a," bg-color-name bg-color-name)
         "colback=white,colframe=white,opacityback=0,opacityframe=0,")
     "boxrule=0pt,sharp corners,breakable,enhanced,boxsep=0pt,"
     "left=4pt,right=4pt,top=4pt,bottom=4pt,width=\\linewidth"))
  ;; wrap in `\@ifundefined{…}{…}{}` so multiple realize calls
  ;; in the same document (each emitting its own env definitions
  ;; via tex-addition) don't redefine the same env and trip
  ;; "Command \TonartBg… already defined" in pdflatex.  the
  ;; defs of `\definecolor` / `\colorlet` are likewise wrapped
  ;; (`\providecolor` doesn't accept the HTML model uniformly,
  ;; so we use the same ifundefined guard).
  (define def-body
    (string-append
     defs
     (format (string-append
              "  \\newenvironment{~a}{%\n"
              "    \\par\\noindent\n"
              "    \\begin{tcolorbox}[~a]%\n"
              "  ~a")
             env-name tcb-opts
             (if fg (format "  \\color{~a}%\n" fg-color-name) ""))
     (string-append
      "  }{%\n"
      "    \\end{tcolorbox}%\n"
      "  }\n")))
  (define env-def
    (string-append
     "\\makeatletter\n"
     (format "\\@ifundefined{~a}{%\n" env-name)
     def-body
     "}{}\n"
     "\\makeatother\n"))
  (values env-name env-def))

;; Scribble style for a `nested-flow` that should be rendered with
;; the given LaTeX env name + the HTML inline styles (for browsers).
;; `tex-bytes` is the full LaTeX preamble (shared across all colors
;; in this document); scribble dedupes `tex-addition`s by content so
;; emitting the same bytes from many styles costs nothing extra.
;; `fg` / `bg` may be `#f`.
(define (bg-section-style env-name fg bg tex-bytes)
  (define style-parts
    (string-append
     (if bg (format "background-color: ~a; " bg) "")
     (if fg (format "color: ~a; " fg) "")
     "padding: 0.5em;"))
  (make-style env-name
              (list (tex-addition tex-bytes)
                    (attributes `((style . ,style-parts))))))

(provide (all-defined-out)
         (all-from-out "../lilypond/lilypond.rkt"
                       "../script/script.rkt")
         title section subsection subsubsection subsubsub*section
         para centered bold italic nested)

;; A paragraph of plain prose.  The realizer turns this into @para{}.
(define-art-object (text [content]))

;; The displayed title of an art-section.  Each section in the
;; programme wraps its body in `(@ [(art-section "id")] ...)`
;; (so an editor can zoom in / out of it by the section coord)
;; and includes a single `(art-title "Display Name")` art-object
;; that the scribble realizer renders as a heading.  The heading
;; level comes from the depth of the surrounding art-section path —
;; one art-section deep → section, two deep → subsection, etc.
(define-art-object (art-title [content]))

;; Per-section background color.  When placed inside an
;; `(@ [(art-section foo)] …)` block, the body of that section is
;; rendered with the given color as its background.  Color may be
;; any CSS color name ("yellow", "tan", "lightblue", …) or a
;; `#RRGGBB` hex string.  Nested sections inherit their parent's
;; color unless they declare their own `bg` (the deepest declaration
;; wins).  Section headings themselves are rendered outside the
;; colored region, since scribble part-starts can't appear inside a
;; nested-flow.
(define-art-object (bg [color]))

;; Per-section foreground (text) color, parallel to `bg`.  A section
;; can declare either, both, or neither; the realizer combines them
;; into a single per-(fg,bg) wrapper so the colored region in
;; PDF/HTML output picks up both.
(define-art-object (fg [color]))

;; An image to embed in the realized scribble doc.  `src` is the
;; file path (resolved like scribble's own `image` — relative to
;; the rendering document).  The realizer emits this as a centered
;; scribble image block.
(define-art-object (image [src]))

;; Arbitrary Racket expression to embed in the realized scribble
;; doc.  The expression is dropped verbatim into the emitted
;; scribble forms (inside a centered paragraph), so it runs in
;; the rendering document's lexical context — meaning anything
;; bound there (2htdp/image's `star`, `beside`, your own helpers,
;; etc.) is available.  Useful for one-off inline figures that
;; don't merit becoming a .png on disk.
(define-art-object (racket [expr]))

;; `(art-slide <content> …)` — the *wrapper*: one slide whose body is
;; pict content, which the realizer drops into a single slideshow
;; `(slide …)` so it comes out with the real slide layout (margins,
;; aspect ratio, background) rather than as a bare figure:
;;
;;   (art-slide (t "Welcome to FARM")
;;              (colorize (filled-rectangle 200 8) "goldenrod"))
;;
;; `(art-slideshow <slideshow-body> …)` — the *embedding*: the body is a
;; verbatim `#lang slideshow` module body, so it may call `slide`
;; itself (with `#:title`, `item`, `para`, staged reveals, …) and define
;; as many slides as it likes:
;;
;;   (art-slideshow
;;     (slide #:title "Come to FARM" (item "one bit songs") (item "heraldry"))
;;     (slide #:title "Now dance"    (t "arms up!")))
;;
;; Both are rendered the same way `music` is: the body is written to a
;; throwaway `#lang slideshow` module and run through
;; `get-slides-as-picts`, which drives slideshow's own slide assembler
;; with a capturing viewer (NOT the presentation app, whose command-line
;; runner would crash the subprocess).  Each resulting slide pict is
;; saved as a PNG and emitted as a centered image, so it flows to the
;; preview pane and the PDF alike.
;;
;; Because the body runs as its own `#lang slideshow` module, it sees
;; the slideshow language and nothing else -- not the program module's
;; requires or helpers.  Slides are self-contained.  Non-visual
;; realizers (chuck, strudel) don't match either object and skip it.
(define-art-object (art-slide [code]))
(define-art-object (art-slideshow [code]))

;; Inline redaction marker for use inside a `line` or
;; `stage-direction` body, e.g.
;;
;;   (line jared "Don't forget: no " (redact "public drinking") ".")
;;
;; The script renderer replaces each `(redact "...")` fragment
;; with a black bar of the same width as the redacted text — the
;; classic redaction look.  The original text is preserved as the
;; element's content (invisible in PDF, transparent in HTML) so it
;; remains selectable / searchable.
(define-art-object (redact [text]))

;; `(transpose-octave n)` — bundled-up version of the four-step
;; standard-library octave-shift chain.  Expands at rewrite time to
;; the equivalent
;;
;;   (context
;;     (note->^o)
;;     (declare-transpose-octave n)
;;     (run-transpose-octave)
;;     (^o->note))
;;
;; `declare-transpose-octave` is just tonart's original
;; `transpose-octave` art object (renamed locally so this rewriter
;; can claim the bare name).  Typical use is inside
;; `rewrite-in-music` so the chain runs against every note in every
;; music block:
;;
;;   (rewrite-in-music (transpose-octave -1))
;;
;; …drops every piece in the program down by an octave.
(define-art-rewriter transpose-octave
  (λ (stx)
    (syntax-parse stx
      [(_ n:number)
       #'(context
           (note->^o)
           (declare-transpose-octave n)
           (run-transpose-octave)
           (^o->note))])))

;; Composition helper: place `item …` at the end of the
;; surrounding `art-section`'s body by giving it an `index`
;; coordinate that's one past the current maximum among the
;; siblings already in that section.  Caller writes
;;   (@ [(art-section ...)] (append item …))
;; and `append` looks at `(current-ctxt)`, finds every expr
;; sitting at the same `art-section` path, takes the max of
;; their `(index …)` coords, and emits the new item at
;; max-index with its last element incremented (so for
;; existing indices ((0 1 0) (0 1 1) (0 1 2)) the appended
;; item lands at (0 1 3)).
(define-art-rewriter append
  (λ (stx)
    (syntax-parse stx
      [(_ item ...)
       (define our-path
         (map syntax->datum (expr-art-section stx)))
       (define matching
         (filter
          (λ (e)
            (equal? (map syntax->datum (expr-art-section e))
                    our-path))
          (current-ctxt)))
       (define indices (filter pair? (map expr-index matching)))
       (define new-ix
         (cond
           [(null? indices) (list 0)]
           [else
            (define max-ix
              (foldl (λ (a b) (if (index<? b a) a b))
                     (car indices)
                     (cdr indices)))
            ;; replace last element with (last + 1)
            (let loop ([acc '()] [xs max-ix])
              (cond
                [(null? (cdr xs))
                 (reverse (cons (add1 (car xs)) acc))]
                [else
                 (loop (cons (car xs) acc) (cdr xs))]))]))
       (with-syntax ([(ix ...)
                      (map (λ (n) (datum->syntax stx n)) new-ix)])
         (qq-art stx (|@| [(index ix ...)] item ...)))])))

;; Insert an item at a specific index N among the surrounding
;; `art-section`'s body, bumping every existing sibling at
;; last-index ≥ N up by one.  Caller writes
;;   (@ [(art-section ...)] (insert 1 item …))
;; and `insert` rewrites to:
;;   - delete-by-id for every sibling at last-index ≥ N,
;;   - re-add each of those siblings with their last index + 1,
;;   - emit the new item at the requested index.
;; This keeps the section's child indices densely packed without
;; the caller having to know how many siblings there are.
(define-art-rewriter insert
  (λ (stx)
    (syntax-parse stx
      [(_ n:number item ...)
       (define n-val (syntax-e #'n))
       (define our-path
         (map syntax->datum (expr-art-section stx)))
       (define siblings
         (filter
          (λ (e)
            (equal? (map syntax->datum (expr-art-section e))
                    our-path))
          (current-ctxt)))
       (define indexed-siblings
         (filter (λ (e) (pair? (expr-index e))) siblings))
       ;; index prefix (everything but the last component) —
       ;; all siblings share it under a well-formed score.
       (define ix-prefix
         (cond
           [(null? indexed-siblings) '()]
           [else (drop-right (expr-index (car indexed-siblings)) 1)]))
       (define new-ix (append ix-prefix (list n-val)))
       (define to-shift
         (filter
          (λ (e)
            (define ix (expr-index e))
            (and (pair? ix) (>= (last ix) n-val)))
          siblings))
       (define delete-forms
         (for/list ([e (in-list to-shift)])
           (delete-expr e)))
       (define readd-forms
         (for/list ([e (in-list to-shift)])
           (define old-ix (expr-index e))
           (define shifted-ix
             (append (drop-right old-ix 1)
                     (list (add1 (last old-ix)))))
           ;; strip index + art-id from the body so the @-merge
           ;; below installs the new index cleanly and the
           ;; re-added expr gets a fresh art-id.
           (define body
             (remove-from-id-ctxt
              (remove-from-id-ctxt e #'index)
              #'art-id))
           (with-syntax ([(ix ...)
                          (map (λ (k) (datum->syntax stx k))
                               shifted-ix)])
             (qq-art stx (|@| [(index ix ...)] #,body)))))
       (with-syntax ([(ix ...)
                      (map (λ (k) (datum->syntax stx k)) new-ix)])
         (qq-art stx
           (|@| ()
             #,@delete-forms
             #,@readd-forms
             (|@| [(index ix ...)] item ...))))])))

;; Per-section explicit ordering of immediate child sections.
;; Indexes are only unique within a single section, so the order
;; *between* sibling sections is otherwise underspecified.  An
;; expr `(layout a b c)` inside `(@ [(art-section P)] …)` says
;; "when rendering P, present its child sections in the order
;;  a b c"; children not mentioned fall back to their
;; first-occurrence order in `(current-ctxt)`.
(define-art-object (layout [children]))

;; Helpers used by insert-section / append-section.  Both
;; rewriters look at the *parent* section path (i.e. the
;; surrounding art-section minus its last component), build a
;; full layout list of the parent's immediate children, splice
;; the new section name in adjacent to the current child, then
;; replace any existing `(layout …)` at the parent path with
;; the spliced list.

;; Immediate child section keys of `parent-path`, in index-sorted
;; first-occurrence order through `(current-ctxt)`.  Mirrors how
;; the realizer's `group-at` would otherwise order them by
;; default.
(define-for-syntax (parent-children-of parent-path)
  (define sorted
    (sort (current-ctxt) index<?
          #:key (λ (e) (define ix (expr-index e))
                       (if (null? ix) (list +inf.0) ix))))
  (define seen '())
  (define rev '())
  (for ([e (in-list sorted)])
    (define p (map syntax->datum (expr-art-section e)))
    (when (and (> (length p) (length parent-path))
               (equal? (take p (length parent-path)) parent-path))
      (define key (list-ref p (length parent-path)))
      (unless (memq key seen)
        (set! seen (cons key seen))
        (set! rev (cons key rev)))))
  (reverse rev))

;; The existing `(layout …)` expr sitting at `parent-path`, or
;; `#f` if none.  Used so we can delete it before emitting our
;; replacement (multiple layouts at the same path would
;; collide in the realizer's section->layout hash).
(define-for-syntax (existing-layout-at parent-path)
  (findf (λ (e)
           (and (syntax-parse e
                  [(h:id _:id ...) (eq? (syntax-e #'h) 'layout)]
                  [_ #f])
                (equal? (map syntax->datum (expr-art-section e))
                        parent-path)))
         (current-ctxt)))

;; The "current" ordering of `parent-path`'s children, taking
;; an existing layout into account if present (declared keys
;; first, then any unlisted ones in authored order — same
;; merge rule as the realizer itself).
(define-for-syntax (current-layout-order parent-path)
  (define authored (parent-children-of parent-path))
  (define existing (existing-layout-at parent-path))
  (cond
    [existing
     (define declared
       (syntax-parse existing
         [(_ c:id ...) (map syntax-e (syntax->list #'(c ...)))]))
     (append (filter (λ (k) (memq k authored)) declared)
             (filter (λ (k) (not (memq k declared))) authored))]
    [else authored]))

;; Splice `new-key` into `order` adjacent to `anchor-key`
;; (before or after).  Idempotent: if `new-key` is already
;; present anywhere in `order`, it's removed first.  If
;; `anchor-key` is missing, `new-key` is appended at the end.
(define-for-syntax (splice-section new-key anchor-key where order)
  (define filtered (filter (λ (k) (not (eq? k new-key))) order))
  (let loop ([xs filtered])
    (cond
      [(null? xs) (list new-key)]
      [(eq? (car xs) anchor-key)
       (case where
         [(before) (cons new-key xs)]
         [(after)  (cons (car xs) (cons new-key (cdr xs)))])]
      [else (cons (car xs) (loop (cdr xs)))])))

;; Emit the replacement for insert-section / append-section:
;; delete any existing `(layout …)` at `parent-path`, then commit
;; a fresh one with the spliced order.  The new layout has to
;; live at `parent-path` *exactly* — not at the rewriter's own
;; surrounding scope (which is `(art-section parent-path… anchor)`,
;; one level deeper).  We can't get there with the normal `@`
;; rewriter because `@` merges into the surrounding scope's
;; coords (so wrapping `(layout …)` in `(@ [(art-section P)] …)`
;; ends up at `(art-section P… anchor P…)` after the name-coord
;; concatenation).  Instead we emit a raw `(context …)` node with
;; its own `id-ctxt` forced to `'()`, and a child `(layout …)`
;; node whose `id-ctxt` carries exactly `(art-section P…)`.
;; `run-art-expr`'s `context` case (`core.rkt`) then merges those
;; two id-ctxts as `'() + (art-section P…) = (art-section P…)`,
;; bypassing the rewriter's surrounding scope entirely.
(define-for-syntax (emit-layout-rewrite stx parent-path new-order)
  (define existing (existing-layout-at parent-path))
  (define delete-forms
    (if existing (list (delete-expr existing)) '()))
  (define layout-id-ctxt
    (cond
      [(null? parent-path) '()]
      [else
       (with-syntax ([(p ...) (map (λ (k) (datum->syntax stx k))
                                   parent-path)])
         (list #'(art-section p ...)))]))
  (define layout-form
    (with-syntax ([(c ...) (map (λ (k) (datum->syntax stx k)) new-order)])
      (set-id-ctxt (quasisyntax/loc stx (layout c ...))
                   layout-id-ctxt)))
  (set-id-ctxt
   (quasisyntax/loc stx (context #,@delete-forms #,layout-form))
   '()))

;; (@ [(art-section P)] (insert-section new #:before next))
;;   → splice `new` immediately before `next` in P's layout.
;; (@ [(art-section P)] (append-section new #:after prev))
;;   → splice `new` immediately after `prev` in P's layout.
;; The anchor (`#:before` / `#:after` key) is explicit so the
;; rewriter doesn't have to be placed inside the anchor section
;; itself — only inside the parent section whose layout it edits.
;; Both rewriters are idempotent and lazy about completeness:
;; the new section's *content* still lives wherever the author
;; puts it, e.g. `(@ [(art-section P new)] …)` somewhere in
;; the same realize call.
(define-art-rewriter insert-section
  (λ (stx)
    (syntax-parse stx
      [(_ name:id #:before anchor:id)
       (define name-val (syntax-e #'name))
       (define anchor-val (syntax-e #'anchor))
       (define parent-path
         (map syntax->datum (expr-art-section stx)))
       (when (null? parent-path)
         (raise-syntax-error 'insert-section
           "must be used inside an (art-section ...)" stx))
       (define order (current-layout-order parent-path))
       (define new-order
         (splice-section name-val anchor-val 'before order))
       (emit-layout-rewrite stx parent-path new-order)])))

(define-art-rewriter append-section
  (λ (stx)
    (syntax-parse stx
      [(_ name:id #:after anchor:id)
       (define name-val (syntax-e #'name))
       (define anchor-val (syntax-e #'anchor))
       (define parent-path
         (map syntax->datum (expr-art-section stx)))
       (when (null? parent-path)
         (raise-syntax-error 'append-section
           "must be used inside an (art-section ...)" stx))
       (define order (current-layout-order parent-path))
       (define new-order
         (splice-section name-val anchor-val 'after order))
       (emit-layout-rewrite stx parent-path new-order)])))

;; per-realizer counter so multiple `music` blocks get unique
;; filenames within a single doc.
(define-for-syntax music-counter (box 0))

(define-for-syntax (next-music-index!)
  (define n (unbox music-counter))
  (set-box! music-counter (add1 n))
  n)

;; native slide canvas the picts are rendered at before scaling to fit
;; a column -- 4:3, generous enough that the downscaled png stays crisp.
(define-for-syntax SLIDE-RENDER-W 960)
(define-for-syntax SLIDE-RENDER-H 720)

;; slides don't want the full column width -- a slide reads as a slide
;; when it's a card sitting in the page, not edge to edge.  Display at
;; this fraction of the fitted width.
(define-for-syntax SLIDE-DISPLAY-FRACTION 0.5)

;; The scr-image scale that fits a slide png (rendered at SLIDE-RENDER-W)
;; into `fraction` of the column's music-width budget.
(define-for-syntax (slide-display-scale music-max-width-px)
  (min 1.0 (/ (* SLIDE-DISPLAY-FRACTION (exact->inexact music-max-width-px))
              SLIDE-RENDER-W)))

;; Normalize an `art-slideshow` body into a `#lang slideshow` module
;; body.  The body may be written two ways, and both are supported:
;;   - nested `art-slide`s (optionally wrapped in `ix--`), the natural
;;     "a slideshow containing slides" spelling -- each becomes a
;;     `(slide …)`;
;;   - raw slideshow forms (`(slide #:title …)`, `define`s, …), passed
;;     through untouched.
;; `ix--` wrappers are flattened away since order here is source order.
;; Flatten `ix--` wrappers and turn nested `art-slide`s into `(slide …)`.
(define-for-syntax (flatten-slide-forms forms)
  (append-map
   (lambda (f)
     (cond
       [(and (pair? f) (eq? (car f) 'ix--)) (flatten-slide-forms (cdr f))]
       [(and (pair? f) (eq? (car f) 'art-slide)) (list (cons 'slide (cdr f)))]
       [else (list f)]))
   forms))

;; Rewrite every `(slide-image "rel/path")` anywhere in the body into a
;; `(bitmap "/abs/path")`.  The path is resolved against `src-dir` (the
;; program module's directory) at build time, so the absolute path is
;; baked into the generated slideshow module -- the pane's render
;; subprocess and the playback deck both find the image regardless of
;; their own working directory, which a relative `bitmap` would not.
(define-for-syntax (resolve-slide-images datum src-dir)
  (cond
    [(and (pair? datum) (eq? (car datum) 'slide-image)
          (pair? (cdr datum)) (string? (cadr datum)))
     (list 'bitmap (path->string (path->complete-path (cadr datum) src-dir)))]
    [(pair? datum)
     (cons (resolve-slide-images (car datum) src-dir)
           (resolve-slide-images (cdr datum) src-dir))]
    [else datum]))

;; `src-dir` is the directory of the program module, used to resolve
;; `slide-image` paths.  At realize time that is the load-relative
;; directory; fall back to the current directory.
(define-for-syntax (slideshow-module-body forms)
  (define src-dir (or (current-load-relative-directory) (current-directory)))
  (map (lambda (f) (resolve-slide-images f src-dir))
       (flatten-slide-forms forms)))

;; Turn a `#lang slideshow` module body (a list of datums) into PNG
;; files, one per `slide`, and return their absolute paths.  Mirrors
;; `render-music-png!`: write a throwaway module beside the output,
;; render it, drop the artifacts on disk, clean up the source.
;;
;; The render runs in a fresh `racket` subprocess, not in this process,
;; and that isolation is load-bearing: `get-slides-as-picts` runs the
;; slideshow module in its own namespace, which instantiates
;; `racket/gui/base` -- and gui may be instantiated only once per
;; process.  The extract process already has it loaded, so calling
;; `get-slides-as-picts` here dies with "cannot instantiate
;; racket/gui/base a second time".  A clean child process has no such
;; conflict, exactly like shelling `music` out to lilypond.
(define-for-syntax (render-slideshow-pngs! body-datums dir n)
  (make-directory* dir)
  (define tmp (build-path dir (format "slideshow-~a.rkt" n)))
  (define prefix (format "slide-~a" n))
  (dynamic-wind
   (lambda ()
     (call-with-output-file tmp #:exists 'replace
       (lambda (p)
         (displayln "#lang slideshow" p)
         (for ([d (in-list body-datums)]) (writeln d p)))))
   (lambda ()
     (define expr
       (string-append
        "(require slideshow/slides-to-picts file/convertible racket/file pict)"
        "(define ps (get-slides-as-picts " (format "~s" (path->string tmp))
        " " (number->string SLIDE-RENDER-W) " " (number->string SLIDE-RENDER-H) " #f))"
        ;; outline each slide: a 2px frame, with a 1px inset around it so
        ;; the border isn't clipped at the png's very edge.
        "(define (outline p) (inset (frame p #:line-width 2) 1))"
        "(for ([p ps] [k (in-naturals)])"
        "  (call-with-output-file (build-path " (format "~s" (if (path? dir) (path->string dir) dir))
        "                          (format \"" prefix "-~a.png\" k))"
        "    #:exists 'replace (lambda (o) (write-bytes (convert (outline p) 'png-bytes) o))))"
        "(displayln (length ps))"))
     (define racket-exe (path->string (find-system-path 'exec-file)))
     (define out (open-output-string))
     (define ok?
       (parameterize ([current-output-port out])
         (system* racket-exe "-e" expr)))
     (define count (or (and ok? (string->number (string-trim (get-output-string out)))) 0))
     (for/list ([k (in-range count)])
       (path->string (path->complete-path
                      (build-path dir (format "~a-~a.png" prefix k))))))
   (lambda ()
     (with-handlers ([(lambda (_) #t) void]) (delete-file tmp)))))

(define-for-syntax (shell-quote s)
  (string-append "'" (regexp-replace* "'" s "'\"'\"'") "'"))

;; pdf width budget — pdf renders pngs at their pixel size interpreted
;; via the file's dpi metadata.  lilypond emits pngs at ~101 dpi, so a
;; 724-wide score is ~7.2", which spills past us-letter's ~6.5" usable
;; width.  we cap effective width at this many pixels and pass the
;; resulting scale factor to scribble's `image`.  the realizer scales
;; this down by #:columns so two-column layouts get ~half-width snippets,
;; three-column gets ~third-width, etc.
(define-for-syntax MAX-MUSIC-WIDTH-PX 620)

;; image art-object width / height budget — pngs handed to the
;; `image` art object generally come from racket/draw / 2htdp/image's
;; save-image, which writes png without any dpi metadata.  pdflatex
;; then interprets them at the default 72 dpi, so a 280-wide bitmap
;; renders at ~3.9" — wider than a two-column page's column.  we cap
;; full-page-width image renders at this many pixels at 72 dpi
;; (≈ 6.0") and divide by #:columns just like music snippets.  we
;; ALSO cap height — without this, square / tall art (e.g. the
;; 1320x1320 buns grid) takes up most of the page even after the
;; width cap.  any bitmap smaller than its column budget on both
;; axes is left at natural size (scale = 1.0).
(define-for-syntax MAX-IMAGE-WIDTH-PX 432)
;; ≈ 0.67" max image height at 72 dpi — tuned so section artwork
;; (buns, lamb, etc.) reads as a small inline icon next to its
;; staff of music rather than a full-width illustration.  bump
;; this up to make images larger; lower it to make them smaller.
(define-for-syntax MAX-IMAGE-HEIGHT-PX 48)

(define-for-syntax (png-dims path)
  (define bm (read-bitmap path))
  (values (send bm get-width) (send bm get-height)))

;; legacy single-value width helper still used by the music
;; lilypond render path (which only needs to constrain width)
(define-for-syntax (png-width path)
  (define-values (w _h) (png-dims path))
  w)

;; render a single (music expr ...) form to PNG and return two values:
;; the relative path that scribble's image[] will reference, and the
;; `#:scale` factor (≤ 1.0) needed to keep it within `max-width-px`.
;;
;; - inner-exprs  : list of syntax (the music body)
;; - dir          : disk dir to write into (created if missing)
;; - n            : counter index for unique naming
;; - title        : string used as the .ly title
;; - max-width-px : effective per-column pixel budget
(define-for-syntax (render-music-png! inner-exprs dir n title max-width-px)
  (make-directory* dir)
  ;; We deliberately do NOT call `realize-art-exprs` here, because it
  ;; resets `lookup-ctxt` to just the music block's inner exprs, which
  ;; would hide top-level art objects (like an outer `(clef bass)`)
  ;; from the lilypond realizer.  Per art's lookup-ctxt protocol,
  ;; lookups are supposed to cross music boundaries; we preserve that
  ;; by parameterizing lookup-ctxt to include both the inner exprs
  ;; and the outer scope visible at this call site.
  (define perf #'music-lilypond-realizer)
  (define real (syntax-local-value perf (λ () #f)))
  (unless real (raise-syntax-error 'render-music-png! "not a realizer" perf))
  (define perf-stx #`(#,perf #:title #,title))
  (define ly-stx
    (parameterize ([current-ctxt inner-exprs]
                   [lookup-ctxt (append inner-exprs (lookup-ctxt))])
      ((realizer/s-body real) perf-stx)))
  (define ly-text (syntax-e ly-stx))
  (define base (format "music-~a" n))
  (define ly-path (build-path dir (format "~a.ly" base)))
  (call-with-output-file ly-path #:exists 'replace
    (λ (p) (display ly-text p)))
  (define out-stem (path->string (build-path dir base)))
  ;; -dcrop crops the output to the actual ink, dropping page padding,
  ;; margins, and headers/footers.  emits <stem>.cropped.png alongside
  ;; the regular <stem>.png.
  (define cmd
    (format "lilypond --png -dcrop --output=~a ~a 2>&1"
            (shell-quote out-stem)
            (shell-quote (path->string ly-path))))
  (unless (system cmd)
    (error 'scribble-realizer "lilypond failed: ~a" cmd))
  (define png-path (format "~a/~a.cropped.png" dir base))
  (define w (png-width png-path))
  (define scale (min 1.0 (/ max-width-px w)))
  (values png-path scale))

;; (scribble-realizer
;;   [#:title <str>]
;;   [#:music-dir <str>])
;;
;; Macro-expands to:
;;   (begin
;;     (title "...")
;;     (para "...")
;;     (centered (image "music-0.png"))
;;     (para "...")
;;     ...)
;;
;; All scribble forms come from scribble/manual + scribble/core.
(define-art-realizer scribble-realizer
  (λ (stx)
    (syntax-parse stx
      [(_ (~alt (~optional (~seq #:title doc-title:str)
                           #:defaults ([doc-title #'"tonart"]))
                (~optional (~seq #:music-dir music-dir:str)
                           #:defaults ([music-dir #'"scribble-resources"]))
                (~optional (~seq #:compact? compact?:boolean)
                           #:defaults ([compact? #'#t]))
                (~optional (~seq #:numbered? numbered?:boolean)
                           #:defaults ([numbered? #'#t]))
                ;; If false, omit the (title ...) form altogether — useful
                ;; when the realized output is being spliced into another
                ;; scribble document that already has its own title.
                (~optional (~seq #:emit-title? emit-title?:boolean)
                           #:defaults ([emit-title? #'#t]))
                ;; Demote heading levels by this many steps.  0 (default)
                ;; means art-section → @section, art-subsection → @subsection,
                ;; etc.  Set to 1 to nest the realized output one level deeper
                ;; (art-section → @subsection, art-subsection → @subsubsection)
                ;; — useful when splicing into a paper that already has a
                ;; section wrapping the realized programme.
                (~optional (~seq #:section-depth section-depth:nat)
                           #:defaults ([section-depth #'0]))
                ;; Number of columns each music PNG is sized to fit
                ;; within.  1 (default) gives full-width snippets; 2
                ;; shrinks them to ~half-width so they fit one column of
                ;; a two-column paper (e.g. acmart sigplan/sigconf), and
                ;; so on.  Does NOT by itself wrap the body in a
                ;; multicols environment — pair with #:multicol? when
                ;; you want the realized body laid out in multiple
                ;; columns of its own.
                (~optional (~seq #:columns columns:number)
                           #:defaults ([columns #'1]))
                ;; If true, wrap the body in a `\begin{multicols}{n}` /
                ;; `\end{multicols}` pair (n = #:columns) in PDF output.
                ;; Use this only when the surrounding document is itself
                ;; single-column; in a two-column class like acmart's
                ;; sigplan you'd end up with four columns total.
                (~optional (~seq #:multicol? multicol?:boolean)
                           #:defaults ([multicol? #'#f]))
                ;; Per-document overrides for the image-bounding box,
                ;; in 72-dpi pixels.  Default to the module-level
                ;; `MAX-IMAGE-WIDTH-PX` / `MAX-IMAGE-HEIGHT-PX` so
                ;; existing call sites are unaffected; bump these
                ;; when a programme has photo / illustration art that
                ;; should print bigger than the small "inline-icon"
                ;; default tuned for the bun / lamb / star graphics.
                (~optional (~seq #:image-width-px image-width-px:nat)
                           #:defaults ([image-width-px #`#,MAX-IMAGE-WIDTH-PX]))
                (~optional (~seq #:image-height-px image-height-px:nat)
                           #:defaults ([image-height-px #`#,MAX-IMAGE-HEIGHT-PX])))
          ...)
       (define title-val (syntax-e #'doc-title))
       (define dir-val (syntax-e #'music-dir))
       (define compact-val (syntax-e #'compact?))
       (define numbered-val (syntax-e #'numbered?))
       (define emit-title-val (syntax-e #'emit-title?))
       (define section-depth-val (syntax-e #'section-depth))
       (define columns-val (max 1 (syntax-e #'columns)))
       (define multicol-val (syntax-e #'multicol?))
       ;; per-column music width budget — full page divided across
       ;; columns, with a touch of slop for the gutter.
       (define music-max-width-px
         (max 60 (inexact->exact (floor (/ MAX-MUSIC-WIDTH-PX columns-val)))))
       ;; per-column image width budget (in 72-dpi pixels) — same
       ;; idea as music-max-width-px but tuned for racket/draw pngs
       ;; that ship without dpi metadata.  See `MAX-IMAGE-WIDTH-PX`
       ;; up top for the rationale.  Uses the per-call override when
       ;; supplied so a programme can opt into bigger photo art.
       (define image-width-px-val (syntax-e #'image-width-px))
       (define image-height-px-val (syntax-e #'image-height-px))
       (define image-max-width-px
         (max 60 (inexact->exact (floor (/ image-width-px-val columns-val)))))
       (define heading-style #`(heading-style-for #,numbered-val))
       ;; Heading constructors at the requested depth.  Scribble has
       ;; no standard numbered 4th level, but `subsubsub*section` works
       ;; (unnumbered) for depth 3; beyond that everything degrades to
       ;; bold prose.
       (define (heading-at depth)
         (case depth
           [(0) #'section] [(1) #'subsection] [(2) #'subsubsection]
           [(3) #'subsubsub*section]
           [else #f]))
       (define section-form    (heading-at section-depth-val))
       (define subsection-form (heading-at (+ section-depth-val 1)))
       (define subsubsection-form (heading-at (+ section-depth-val 2)))
       ;; Emit a (heading-form #:style heading-style content) call,
       ;; falling back to bold prose if there's no constructor for this
       ;; depth, and omitting #:style for subsubsub*section which only
       ;; takes #:tag.
       (define (heading-form-for form heading-style content-stx)
         (cond
           [(not form)
            #`(para (bold #,(syntax-e content-stx)))]
           [(free-identifier=? form #'subsubsub*section)
            #`(#,form #,(syntax-e content-stx))]
           [else
            #`(#,form #:style #,heading-style #,(syntax-e content-stx))]))
       ;; reset the per-realize counter so the same doc realized twice
       ;; (or multiple docs in one process) produces stable names.
       (set-box! music-counter 0)

       ;; sort by `index` coordinate so output respects authoring
       ;; order even after rewriters have shuffled the context.  the
       ;; program author is responsible for putting an `(ix-- …)`
       ;; around their score body so each top-level expression carries
       ;; an `index`.  exprs without an index (`expr-index` returns
       ;; '()) sort to the end via the +inf.0 sentinel key.
       (define (expr-key e)
         (define ix (expr-index e))
         (if (null? ix) (list +inf.0) ix))
       (define sorted-ctxt
         (sort (current-ctxt) index<? #:key expr-key))

       ;; Depth of `expr`'s surrounding art-section coord path.
       ;; Used by art-title to decide which scribble heading form
       ;; (section / subsection / subsubsection / …) to emit.
       (define (art-section-depth expr)
         (length (expr-art-section expr)))

       ;; Section path of `expr`, as a datum list (e.g. '(look-mom
       ;; row-row-row)).  Used as a hash key when grouping by `bg` /
       ;; `fg` declarations.
       (define (expr-section-path expr)
         (map syntax->datum (expr-art-section expr)))

       ;; Walk `(current-ctxt)` for `bg` / `fg` color declarations
       ;; and build hashes from section path → color.  Each
       ;; declaration colors its own art-section and all its
       ;; descendants (the descendants pick up the colors in
       ;; `colors-for-expr` by walking up their path).
       (define (collect-colors head)
         (for/hash ([e (in-list (current-ctxt))]
                    #:when (syntax-parse e
                             [(h:id _:str) (eq? (syntax-e #'h) head)]
                             [_ #f]))
           (values (expr-section-path e)
                   (syntax-parse e [(_ c:str) (syntax-e #'c)]))))
       (define section->bg-color (collect-colors 'bg))
       (define section->fg-color (collect-colors 'fg))

       ;; section-path → list of child section-name symbols (the
       ;; author-declared order for that section's immediate
       ;; subsections).  Built from any `(layout a b c …)` exprs
       ;; sitting inside `(@ [(art-section P)] …)`.
       (define section->layout
         (for/hash ([e (in-list (current-ctxt))]
                    #:when (syntax-parse e
                             [(h:id _:id ...)
                              (eq? (syntax-e #'h) 'layout)]
                             [_ #f]))
           (values (expr-section-path e)
                   (syntax-parse e
                     [(_ c:id ...)
                      (map syntax-e (syntax->list #'(c ...)))]))))

       ;; Closest declaration of `kind` (#'bg or #'fg) walking up
       ;; `expr`'s section path.  `#f` if none.
       (define (lookup-color path table)
         (let loop ([p path])
           (cond [(hash-ref table p #f) => values]
                 [(null? p) #f]
                 [else (loop (drop-right p 1))])))

       ;; `(cons fg bg)` for `expr`; either may be #f.  Used both to
       ;; decide whether to wrap in a colored nested-flow and to key
       ;; the per-(fg,bg) LaTeX env table.
       (define (colors-for-expr expr)
         (define path (expr-section-path expr))
         (cons (lookup-color path section->fg-color)
               (lookup-color path section->bg-color)))

       ;; Distinct (fg . bg) pairs that actually need a wrapper —
       ;; skip the (#f . #f) "uncolored" pair.  Pairs come from
       ;; *direct* `(bg ...)` / `(fg ...)` declarations on each
       ;; section path; the realizer's tree-based renderer wraps
       ;; each declared section in exactly one nested-flow with
       ;; that section's own colors (inherited colors are realized
       ;; visually by an ancestor's wrapper containing the
       ;; descendant, not by re-coloring the descendant).
       (define unique-pairs
         (remove-duplicates
          (filter (λ (p) (or (car p) (cdr p)))
                  (for/list ([path (in-list
                                    (remove-duplicates
                                     (append (hash-keys section->bg-color)
                                             (hash-keys section->fg-color))))])
                    (cons (hash-ref section->fg-color path #f)
                          (hash-ref section->bg-color path #f))))))

       ;; Map: (fg . bg) pair → (cons env-name env-tex-source).
       (define pair->env
         (for/hash ([p (in-list unique-pairs)]
                    [i (in-naturals)])
           (define-values (env-name env-def)
             (bg-color-env-def (car p) (cdr p) i))
           (values p (cons env-name env-def))))

       ;; Full LaTeX preamble for all colored sections: tcolorbox
       ;; setup + every per-(fg,bg) env definition.  Emitted once
       ;; via tex-addition (scribble dedupes additions by content,
       ;; so sharing the same bytes across many styles is cheap).
       (define bg-tex-bytes
         (cond
           [(hash-empty? pair->env) #""]
           [else
            (string->bytes/utf-8
             (apply string-append
                    "\\usepackage[most]{tcolorbox}\n"
                    (for/list ([entry (in-hash-values pair->env)])
                      (cdr entry))))]))

       ;; `#t` if `expr` is inside a section that has either a `bg`
       ;; or `fg` declared (anywhere up its section path).
       (define (colored-expr? expr)
         (define p (colors-for-expr expr))
         (or (car p) (cdr p)))

       ;; A "heading" expr is one that produces a scribble part-start
       ;; (section / subsection / …), which can't live inside a
       ;; nested-flow.  We break colored runs on these so the
       ;; surrounding `nested` doesn't try to swallow them.  An
       ;; art-title in a *colored* section is rendered as a styled
       ;; paragraph (see `emit-form`) instead of a part-start, so it
       ;; doesn't count as a heading here.
       (define (heading-expr? expr)
         (syntax-parse expr
           [({~literal art-title} _:str)
            (not (colored-expr? expr))]
           [_ #f]))

       ;; Per-expr form emission.  Returns #f for exprs that produce
       ;; no output (e.g. `bg`, `fg`, unknown coordinates).
       (define (emit-form expr)
         (syntax-parse expr
           [({~literal text} c:str)
            #`(para #,(syntax-e #'c))]
           ;; art-title in a colored section: a scribble part-start
           ;; (a real heading) can't live inside the wrapping
           ;; nested-flow that paints the background, so this is a
           ;; *paragraph* that shares the section's color.  To keep it
           ;; heading-*sized*, the para carries a `tonart-colored-title`
           ;; style property tagged with the heading depth: the preview
           ;; renderer promotes such a para to a heading block (so
           ;; layout gives it the heading scale), and `larger` gives the
           ;; PDF the same bump.  The style name stays #f so LaTeX
           ;; doesn't hunt for a command by that name.
           [({~literal art-title} c:str)
            #:when (colored-expr? expr)
            ;; `+1`: a scribble `section` renders at display-depth 1
            ;; (the document title occupies depth 0), so the promoted
            ;; heading has to match, or a colored section title comes
            ;; out at the larger document-title scale.
            (define d (max 0 (sub1 (art-section-depth expr))))
            #`(para #:style (make-style
                             #f (list (list 'tonart-colored-title
                                            #,(+ section-depth-val d 1))))
                    (larger (bold #,(syntax-e #'c))))]
           ;; art-title renders as a heading at depth =
           ;; length of its art-section path minus one
           ;; (so an art-title in `(@ [(art-section ...)] …)`
           ;; is a top-level @section, one inside a nested
           ;; pair of art-section coords is an @subsection,
           ;; etc.) — plus the realizer's overall
           ;; #:section-depth offset.
           [({~literal art-title} c:str)
            (define d (max 0 (sub1 (art-section-depth expr))))
            (heading-form-for (heading-at (+ section-depth-val d))
                              heading-style
                              #'c)]
           [({~literal music} inner ...)
            (define n (next-music-index!))
            (define-values (png-path scale)
              (render-music-png!
               (syntax->list #'(inner ...))
               dir-val n "" music-max-width-px))
            #`(centered (scr-image #,png-path #:scale #,scale))]
           [({~literal image} src:str)
            ;; resolve relative paths against the realize call's
            ;; source directory so the image can be found at render
            ;; time regardless of the latex working dir.
            (define raw (syntax-e #'src))
            (define abs
              (cond
                [(absolute-path? raw) raw]
                [(current-load-relative-directory)
                 (path->string
                  (simplify-path
                   (build-path (current-load-relative-directory) raw)))]
                [else raw]))
            ;; auto-scale oversized pngs so they fit within a
            ;; single column at 72 dpi — bounded on BOTH axes so
            ;; tall / square artwork doesn't take over the page.
            ;; png-dims fails silently if the file is missing —
            ;; in that case we just emit the image unscaled and
            ;; let scribble surface the error.
            (define scale
              (with-handlers ([exn:fail? (λ (_) 1.0)])
                (define-values (w h) (png-dims abs))
                (min 1.0
                     (/ image-max-width-px w)
                     (/ image-height-px-val h))))
            #`(centered (scr-image #,abs #:scale #,scale))]
           [({~literal racket} expr ...)
            ;; emit the verbatim racket expression(s) inside a
            ;; centered paragraph.  evaluation happens at scribble
            ;; render time using the bindings of the document
            ;; that contains the realize call.
            #'(centered (para expr ...))]
           [({~literal art-slide} code ...)
            ;; wrapper: drop the content into one `(slide …)` and render
            ;; that as a styled slide.
            (define n (next-music-index!))
            (define body (list (cons 'slide (map syntax->datum
                                                 (syntax->list #'(code ...))))))
            (define pngs (render-slideshow-pngs! body dir-val n))
            (define scale (slide-display-scale music-max-width-px))
            (if (pair? pngs)
                #`(centered (scr-image #,(car pngs) #:scale #,scale))
                #`(para "[empty slide]"))]
           [({~literal art-slideshow} code ...)
            ;; embedding: a deck.  The body may nest `art-slide`s or be
            ;; raw slideshow code; either way it renders as many styled
            ;; slides as it defines.
            (define n (next-music-index!))
            (define body (slideshow-module-body
                          (map syntax->datum (syntax->list #'(code ...)))))
            (define pngs (render-slideshow-pngs! body dir-val n))
            (define scale (slide-display-scale music-max-width-px))
            #`(splice
               (list #,@(for/list ([p (in-list pngs)])
                          #`(centered (scr-image #,p #:scale #,scale)))))]
           [({~literal script} inner ...)
            ;; gather the line/direction art-objects inside
            ;; this script block.  each one becomes a single
            ;; @para — `line` is rendered "**ALICE.** text",
            ;; `direction` is rendered as italic prose.  Both
            ;; accept a body of mixed string fragments and
            ;; `(redact "...")` markers; each redact fragment is
            ;; replaced with a black bar of the same width.
            (define (frag->stx f)
              (syntax-parse f
                [s:str (syntax-e #'s)]
                [({~literal redact} t:str)
                 #`(elem #:style redact-style #,(syntax-e #'t))]
                [other #'other]))
            (define paras
              (filter values
                (for/list ([item (in-list (syntax->list #'(inner ...)))])
                  (syntax-parse item
                    [({~literal line} ch:id frag ...)
                     (define name
                       (string-upcase (symbol->string (syntax-e #'ch))))
                     (define content
                       (map frag->stx (syntax->list #'(frag ...))))
                     #`(para (bold #,(string-append name "."))
                             " "
                             #,@content)]
                    [({~literal stage-direction} frag ...)
                     (define content
                       (map frag->stx (syntax->list #'(frag ...))))
                     #`(para (italic #,@content))]
                    [_ #f]))))
            #`(splice (list #,@paras))]
           [_ #f]))

       ;; Pair each expr with its emitted form (possibly #f).  The
       ;; subsequent grouping logic walks these in their sorted-ctxt
       ;; order and recursively nests them by art-section path.
       (define expr+form
         (for/list ([expr (in-list sorted-ctxt)])
           (cons expr (emit-form expr))))

       ;; Build a tree mirroring the art-section hierarchy.
       ;; - Each `'leaf` node holds one emitted form.
       ;; - Each `'section` node carries a section path and a list of
       ;;   child nodes.
       ;; Items belonging to the same section path are collected
       ;; *globally* (not just from a consecutive run of the sorted
       ;; ctxt), so a rewriter that drops a new item into an
       ;; already-emitted section — e.g. `(append (image "stars.png"))`
       ;; inside `(@ [(art-section look-mom twinkle)] …)` — lands
       ;; back inside that section's bucket rather than starting a
       ;; new sibling bucket at the end.  Each parent's child
       ;; buckets are ordered by:
       ;;   1. the parent's `layout` declaration, if any, then
       ;;   2. first-occurrence order in the index-sorted ctxt
       ;;      (which for `ix--`-tagged scores is min-index order).
       ;; Items *within* a bucket keep their sorted-ctxt order.
       (define (group-at items section-path depth)
         (define leaves-rev '())
         (define buckets (make-hash))
         (define seen-rev '())
         (for ([it (in-list items)])
           (define p (expr-section-path (car it)))
           (cond
             [(<= (length p) depth)
              (set! leaves-rev (cons it leaves-rev))]
             [else
              (define key (list-ref p depth))
              (unless (hash-has-key? buckets key)
                (hash-set! buckets key '())
                (set! seen-rev (cons key seen-rev)))
              (hash-update! buckets key (λ (l) (cons it l)))]))
         (define authored-order (reverse seen-rev))
         (define layout-order (hash-ref section->layout section-path #f))
         (define ordered-keys
           (cond
             [layout-order
              ;; layout-listed children first (in layout order, but
              ;; only those that actually have content), then any
              ;; un-listed children in authored order.
              (append (filter (λ (k) (memq k authored-order)) layout-order)
                      (filter (λ (k) (not (memq k layout-order)))
                              authored-order))]
             [else authored-order]))
         ;; `art-title` is a section's heading; the demo papers
         ;; assume it sits at the top of its bucket regardless of
         ;; how the author orders the rest of the section's
         ;; content (so authors don't have to remember to put it
         ;; first in `ix--`, and so a layout-driven score with no
         ;; `ix--` over its leaves still gets a sensible heading
         ;; position).  Pull any art-title leaves to the front of
         ;; the leaf list, preserving the original order of both
         ;; partitions among themselves.
         (define ordered-leaves
           (let-values ([(titles others)
                         (partition
                           (λ (it)
                             (syntax-parse (car it)
                               [({~literal art-title} _:str) #t]
                               [_ #f]))
                           (reverse leaves-rev))])
             (append titles others)))
         (define leaf-nodes
           (map (λ (it) (cons 'leaf (cdr it))) ordered-leaves))
         (define section-nodes
           (for/list ([key (in-list ordered-keys)])
             (define its (reverse (hash-ref buckets key)))
             (define sub-path (append section-path (list key)))
             (list 'section sub-path
                   (group-at its sub-path (add1 depth)))))
         (append leaf-nodes section-nodes))

       (define tree (group-at expr+form '() 0))

       ;; Convert the tree to a list of scribble forms.  Each
       ;; `'section` node with a directly-declared `bg`/`fg` gets
       ;; wrapped in a `nested` with the per-(fg,bg) style; sections
       ;; that only *inherit* a color from an ancestor get no
       ;; wrapper of their own (the ancestor's wrapper is already
       ;; coloring them).  Empty sections are dropped.
       (define (tree->forms nodes)
         (filter values
                 (for/list ([n (in-list nodes)])
                   (cond
                     [(eq? (car n) 'leaf) (cdr n)]
                     [else
                      (define path (cadr n))
                      (define children (tree->forms (caddr n)))
                      (define bg (hash-ref section->bg-color path #f))
                      (define fg (hash-ref section->fg-color path #f))
                      (cond
                        [(null? children) #f]
                        [(or fg bg)
                         (define env-name
                           (car (hash-ref pair->env (cons fg bg))))
                         #`(nested #:style (bg-section-style
                                             #,env-name
                                             #,(if fg fg #'#f)
                                             #,(if bg bg #'#f)
                                             #,bg-tex-bytes)
                                   #,@children)]
                        [(= 1 (length children)) (car children)]
                        [else #`(splice (list #,@children))])]))))

       (define forms (tree->forms tree))

       (define title-form
         (cond [(not emit-title-val) #f]
               [compact-val #`(title #:style compact-style #,title-val)]
               [else #`(title #,title-val)]))
       ;; scribble's `splice` flattens its run into the surrounding
       ;; doc body.  plain (list ...) gets rendered as one item, so we
       ;; need splice for multiple top-level paras / images.  When
       ;; #:multicol? is set we bookend the body with two empty
       ;; paragraphs whose styles emit \begin{multicols}{n} /
       ;; \end{multicols} into the LaTeX output (HTML ignores the
       ;; unknown style names).  This is the only way to get a
       ;; multicols region that contains section headings, since
       ;; scribble's nested-flow doesn't accept parts (only blocks).
       (define body-forms
         (cond [(and multicol-val (> columns-val 1))
                (append (list #'(para #:style two-col-begin-style))
                        forms
                        (list #'(para #:style two-col-end-style)))]
               [else forms]))
       (if title-form
           #`(splice (list #,title-form #,@body-forms))
           #`(splice (list #,@body-forms)))])))
