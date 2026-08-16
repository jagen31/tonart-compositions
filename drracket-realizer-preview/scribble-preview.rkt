#lang racket/base

;; Turn a scribble document (a `part`) into a "display list": a plain
;; s-expression describing headings, styled text runs, and images.
;;
;; The point of the display list is that it survives a `write`/`read`
;; round trip, so the document can be rendered in a subprocess (where
;; expanding it is allowed to take ten seconds, shell out to lilypond,
;; or crash) and drawn in DrRacket's GUI process.
;;
;; We get there by subclassing scribble's own `render%` rather than
;; walking the `part` structs directly.  That matters: the base
;; renderer runs the real traverse / collect / resolve passes, so
;; `delayed-element`s, cross references, and `@examples` output are
;; already resolved by the time our methods see them.  Walking the raw
;; structs would hit unresolved elements and error out.
;;
;; Grammar:
;;
;;   RESULT ::= (ok BLOCK ...)
;;            | (err MESSAGE LOG)
;;
;;   BLOCK  ::= (heading DEPTH ALIGN SPAN ...)   ; DEPTH 0 = document title
;;            | (para ALIGN SPAN ...)
;;            | (nested BG FG BLOCK ...)         ; a colored region
;;            | (items BULLET (BLOCK ...) ...)
;;            | (table (CELL ...) ...)
;;
;;   CELL   ::= cont | (BLOCK ...)
;;   SPAN   ::= (t STRING BOLD? ITALIC? TT? COLOR)
;;            | (img PATH SCALE)                 ; image on disk
;;            | (imgb BYTES SCALE)               ; converted inline, e.g. a pict
;;            | (br)
;;   ALIGN  ::= left | center | right
;;   COLOR  ::= #f | STRING                      ; CSS name or #RRGGBB
;;   BULLET ::= bullet | ordered

(require racket/class
         racket/list
         racket/string
         file/convertible
         setup/collects
         scribble/core
         scribble/base-render
         scribble/html-properties)

(provide doc->display-list)

;; ---------------------------------------------------------------------------
;; emit state
;;
;; The base renderer's methods communicate by returning lists, which is
;; awkward for building a tree.  We ignore the return values and
;; accumulate into these parameters instead, exactly the way
;; scribble/text-render accumulates into `current-output-port`.
;; ---------------------------------------------------------------------------

(define current-blocks (make-parameter #f))  ; box of reversed BLOCKs
(define current-spans (make-parameter #f))   ; box of reversed SPANs, or #f
(define current-align (make-parameter 'left))

(define current-bold (make-parameter #f))
(define current-italic (make-parameter #f))
(define current-tt (make-parameter #f))
(define current-color (make-parameter #f))

(define (emit-block! b)
  (define bx (current-blocks))
  (when bx (set-box! bx (cons b (unbox bx)))))

(define (emit-span! s)
  (define bx (current-spans))
  (when bx (set-box! bx (cons s (unbox bx)))))

;; Run `thunk` with a fresh block accumulator and return what it emitted.
(define (capture-blocks thunk)
  (define bx (box '()))
  (parameterize ([current-blocks bx]) (thunk))
  (reverse (unbox bx)))

;; Same, for inline content.
(define (capture-spans thunk)
  (define bx (box '()))
  (parameterize ([current-spans bx]) (thunk))
  (merge-spans (reverse (unbox bx))))

;; Adjacent text runs with identical styling become one run.  Scribble
;; emits a lot of one-character content, and every run costs a
;; `change-style` call on the GUI side.
(define (merge-spans spans)
  (let loop ([l spans])
    (cond
      [(null? l) '()]
      [(and (pair? (cdr l))
            (eq? 't (caar l))
            (eq? 't (car (cadr l)))
            (equal? (cddar l) (cdr (cdr (cadr l)))))
       (define a (car l))
       (define b (cadr l))
       (loop (cons (list* 't (string-append (cadr a) (cadr b)) (cddr a))
                   (cddr l)))]
      [else (cons (car l) (loop (cdr l)))])))

;; ---------------------------------------------------------------------------
;; text
;; ---------------------------------------------------------------------------

;; Scribble's `---`/`--`/quote conventions.  The HTML and LaTeX
;; renderers apply these via `get-substitutions`; the base renderer
;; does not, so we do it here.  Skipped inside `tt` so code samples
;; keep their literal characters.
(define (substitute str)
  (if (current-tt)
      str
      (let* ([s (regexp-replace* #rx"---" str "—")]
             [s (regexp-replace* #rx"--" s "–")]
             [s (regexp-replace* #rx"``" s "“")]
             [s (regexp-replace* #rx"''" s "”")]
             [s (regexp-replace* #rx"'" s "’")])
        s)))

(define entities
  (hash 'mdash "—" 'ndash "–"
        'ldquo "“" 'rdquo "”"
        'lsquo "‘" 'rsquo "’"
        'lang "⟨" 'rang "⟩"
        'rarr "→" 'larr "←"
        'prime "′" 'nbsp " "
        'hellip "…" 'alpha "α" 'infin "∞"))

(define (emit-text! str)
  (unless (string=? str "")
    (emit-span! (list 't str (current-bold) (current-italic)
                      (current-tt) (current-color)))))

;; ---------------------------------------------------------------------------
;; styles
;; ---------------------------------------------------------------------------

(define (style-name* s) (if (style? s) (style-name s) s))
(define (style-props* s) (if (style? s) (style-properties s) '()))

;; The heading depth carried by an art-title-in-a-colored-section
;; paragraph (emitted by scribble.rkt), or #f for an ordinary para.
(define (colored-title-depth s)
  (for/or ([prop (in-list (style-props* s))])
    (and (pair? prop) (eq? (car prop) 'tonart-colored-title) (cadr prop))))

;; Colors from scribble's manual-racket.css, so code in the preview
;; looks like code in the rendered HTML.
(define rkt-colors
  (hash "RktSym" "#262626"  "RktVal" "#228b22"  "RktPn" "#843c24"
        "RktMeta" "#262626" "RktMod" "#262626"  "RktKw" "#262626"
        "RktVar" "#262626"  "RktCmt" "#c2741f"  "RktRes" "#0000af"
        "RktOut" "#960096"  "RktErr" "#cc0000"  "RktIn" "#cc6633"
        "RktValLink" "#0000cc" "RktModLink" "#0000cc"
        "RktStxLink" "#262626" "RktSymDef" "#262626" "RktValDef" "#0000cc"))

(define (color-property-value p)
  (define c (color-property-color p))
  (if (list? c)
      (format "#~a" (apply string-append
                           (for/list ([b (in-list c)])
                             (define s (number->string b 16))
                             (if (= 1 (string-length s)) (string-append "0" s) s))))
      c))

;; Apply an element's style to the ambient text style and run `thunk`.
(define (with-element-style st thunk)
  (define name (style-name* st))
  (define props (style-props* st))
  (define rkt? (and (string? name) (regexp-match? #rx"^Rkt" name)))
  (define color
    (or (for/or ([p (in-list props)])
          (and (color-property? p) (color-property-value p)))
        (and (string? name) (hash-ref rkt-colors name #f))
        (current-color)))
  (parameterize ([current-bold (or (memq name '(bold)) (current-bold))]
                 [current-italic (or (memq name '(italic))
                                     (equal? name "RktVar")
                                     (current-italic))]
                 [current-tt (or (memq name '(tt sf hspace)) rkt? (current-tt))]
                 [current-color color])
    (thunk)))

;; A nested-flow / part style can carry section colors two ways: as
;; scribble's own color properties, or -- which is what
;; compositions/scribble/scribble.rkt's `bg-section-style` does -- as
;; an HTML `style` attribute holding CSS.
(define (style-colors st)
  (define props (style-props* st))
  (define bg (for/or ([p (in-list props)])
               (and (background-color-property? p)
                    (let ([c (background-color-property-color p)])
                      (if (list? c) (color-property-value (color-property c)) c)))))
  (define fg (for/or ([p (in-list props)])
               (and (color-property? p) (color-property-value p))))
  (define css
    (for/or ([p (in-list props)])
      (and (attributes? p)
           (let ([a (assq 'style (attributes-assoc p))])
             (and a (cdr a))))))
  (define (css-field key)
    (and css
         (let ([m (regexp-match (pregexp (format "(?:^|;)\\s*~a\\s*:\\s*([^;]+)" key)) css)])
           (and m (string-trim (cadr m))))))
  (values (or bg (css-field "background-color"))
          (or fg (css-field "(?<!-)color"))))

;; ---------------------------------------------------------------------------
;; the renderer
;; ---------------------------------------------------------------------------

(define (part-hidden? p) (memq 'hidden (style-properties (part-style p))))

(define preview-render%
  (class render%
    (inherit render-block render-flow render-part number-depth)

    (define/override (current-render-mode) '(text))

    ;; Overriding `render-part-content` rather than `render-part` keeps
    ;; the base class's tag-prefix / part-context parameterizations,
    ;; which resolve-time elements rely on.
    (define/override (render-part-content d ri)
      (unless (part-hidden? d)
        (when (part-title-content d)
          (define depth (number-depth (collected-info-number (part-collected-info d ri))))
          (define spans (capture-spans (lambda () (render-content (part-title-content d) d ri))))
          (unless (null? spans)
            (emit-block! (list* 'heading depth (if (zero? depth) 'center 'left) spans)))))
      (render-flow (part-blocks d) d ri #f)
      (for ([s (in-list (part-parts d))]) (render-part s ri))
      null)

    (define/override (render-paragraph p part ri)
      (define spans (capture-spans (lambda () (super render-paragraph p part ri))))
      (unless (null? spans)
        ;; An art-title in a colored section arrives as a paragraph (it
        ;; can't be a real heading inside the coloring nested-flow), but
        ;; carries a `tonart-colored-title` property with its depth.
        ;; Promote it to a heading block so it renders at heading scale.
        (define depth (colored-title-depth (paragraph-style p)))
        (if depth
            (emit-block! (list* 'heading depth 'left spans))
            (emit-block! (list* 'para (current-align) spans))))
      null)

    (define/override (render-nested-flow i part ri starting-item?)
      (define st (nested-flow-style i))
      (define name (style-name* st))
      (define-values (bg fg) (style-colors st))
      (cond
        [(equal? name "SCentered")
         (parameterize ([current-align 'center])
           (super render-nested-flow i part ri starting-item?))]
        [(or bg fg)
         (define blocks
           (capture-blocks (lambda () (super render-nested-flow i part ri starting-item?))))
         (emit-block! (list* 'nested bg fg blocks))
         null]
        [else (super render-nested-flow i part ri starting-item?)]))

    (define/override (render-itemization i part ri)
      (define bullet (if (eq? 'ordered (style-name* (itemization-style i))) 'ordered 'bullet))
      (define rows
        (for/list ([f (in-list (itemization-blockss i))])
          (capture-blocks (lambda () (render-flow f part ri #t)))))
      (emit-block! (list* 'items bullet rows))
      null)

    (define/override (render-table i part ri starting-item?)
      (define rows
        (for/list ([row (in-list (table-blockss i))])
          (for/list ([cell (in-list row)])
            (if (eq? cell 'cont)
                'cont
                (capture-blocks (lambda () (render-block cell part ri #f)))))))
      (emit-block! (cons 'table rows))
      null)

    ;; Content.  `image-element` and convertible values have to be
    ;; caught before the base class gets them: it would render the
    ;; image's alt text, or `~s` the value.
    (define/override (render-content i part ri)
      (cond
        [(image-element? i)
         ;; Paths are relative to the rendering document; `current-directory`
         ;; is that directory (extract.rkt sets it), so complete them here
         ;; rather than making the GUI side guess.
         (define p (image-element-path i))
         (define resolved
           (cond
             [(path? p) p]
             [(string? p) (string->path p)]
             [else (collects-relative->path p)]))
         (emit-span! (list 'img
                           (path->string (path->complete-path resolved))
                           (exact->inexact (image-element-scale i))))
         null]
        [(and (element? i) (not (render-element? i)))
         (case (style-name* (element-style i))
           [(newline) (emit-span! '(br)) null]
           [else (with-element-style (element-style i)
                   (lambda () (super render-content i part ri)))])]
        [(and (convertible? i) (not (element? i)) (not (string? i)) (not (list? i))
              (convert i 'png-bytes #f))
         => (lambda (bs) (emit-span! (list 'imgb bs 1.0)) null)]
        [else (super render-content i part ri)]))

    (define/override (render-other i part ri)
      (cond
        [(string? i) (emit-text! (substitute i))]
        [(symbol? i) (emit-text! (hash-ref entities i (lambda () (format "~a" i))))]
        [(and (number? i) (exact-nonnegative-integer? i))
         (emit-text! (string (integer->char i)))]
        [else (emit-text! (format "~a" i))])
      null)

    (super-new)))

;; Render `doc` and return its BLOCKs.
;;
;; `refer-to-existing-files` keeps `install-file` from copying images
;; into a build directory -- image paths come back pointing at the
;; files lilypond already wrote, which is exactly what the GUI needs.
(define (doc->display-list doc)
  (define r (new preview-render%
                 [dest-dir #f]
                 [refer-to-existing-files #t]))
  (define ds (list doc))
  (define fns (list #f))
  (define fp (send r traverse ds fns))
  (define info (send r collect ds fns fp))
  (define ri (send r resolve ds fns info))
  (capture-blocks (lambda () (send r render ds fns ri))))
