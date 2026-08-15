#lang racket/base

;; Draw a display list (see scribble-preview.rkt) into a `text%`.
;;
;; A `text%` buys us scrolling, word wrapping, selection, copy, and
;; find for free, which a hand-rolled canvas would not.  What it does
;; not have is paragraph alignment, so centering is done by measuring
;; the line and inserting a zero-height spacer snip of the exact
;; leftover width.  That only works if the layout knows the pane
;; width, which is why the caller re-runs this whole function on
;; resize -- the display list is cached, so a relayout is cheap.

(require racket/class
         racket/draw
         racket/list
         racket/gui/base)

(provide preview-text%
         render-display-list!
         render-error!
         render-message!)

;; ---------------------------------------------------------------------------
;; the editor
;;
;; A `change-style` background only colors the glyphs, which makes a
;; colored section look like highlighter pen rather than a tinted
;; block.  Painting the bands ourselves in `on-paint` -- before the
;; text, spanning the full pane width -- gives the same look the PDF
;; has.
;; ---------------------------------------------------------------------------

(define preview-text%
  (class text%
    (super-new)
    (inherit position-location last-position)

    (define bands '())   ; list of (start end color%)

    (define/public (set-bands! bs) (set! bands bs))
    (define/public (clear-bands!) (set! bands '()))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (when (and before? (pair? bands))
        (define old-brush (send dc get-brush))
        (define old-pen (send dc get-pen))
        (send dc set-pen "black" 0 'transparent)
        (for ([b (in-list bands)])
          (define start (car b))
          (define end (cadr b))
          (define color (caddr b))
          (define ytop (box 0.0))
          (define ybot (box 0.0))
          (define xb (box 0.0))
          (position-location start xb ytop #t)
          (position-location (max start (sub1 end)) xb ybot #f)
          (define y0 (unbox ytop))
          (define y1 (unbox ybot))
          (when (> y1 y0)
            (send dc set-brush color 'solid)
            (send dc draw-rectangle (+ dx left) (+ dy y0) (- right left) (- y1 y0))))
        (send dc set-brush old-brush)
        (send dc set-pen old-pen))
      (super on-paint before? dc left top right bottom dx dy draw-caret))))

(define base-size 12)

;; Relative heading sizes, indexed by depth (0 = document title).
(define heading-scales '#(1.9 1.45 1.2 1.08))

;; ---------------------------------------------------------------------------
;; a zero-height spacer, for centering
;; ---------------------------------------------------------------------------

(define spacer-snip%
  (class snip%
    (init-field [spacer-width 0.0])
    (super-new)
    (define/override (get-extent dc x y [w #f] [h #f] [descent #f]
                                 [space #f] [lspace #f] [rspace #f])
      (define (b! bx v) (when bx (set-box! bx v)))
      (b! w (exact->inexact spacer-width))
      (b! h 0.0) (b! descent 0.0) (b! space 0.0) (b! lspace 0.0) (b! rspace 0.0))
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (void))
    (define/override (copy) (new spacer-snip% [spacer-width spacer-width]))))

;; ---------------------------------------------------------------------------
;; fonts and colors
;; ---------------------------------------------------------------------------

(define (make-font size bold? italic? tt?)
  (make-object font%
    (max 1 (inexact->exact (round size)))
    (if tt? 'modern 'default)
    (if italic? 'italic 'normal)
    (if bold? 'bold 'normal)))

(define (make-delta size bold? italic? tt? color)
  (define d (new style-delta%))
  (send d set-size-mult 0.0)
  (send d set-size-add (max 1 (inexact->exact (round size))))
  (send d set-family (if tt? 'modern 'default))
  (send d set-weight-on (if bold? 'bold 'normal))
  (send d set-style-on (if italic? 'italic 'normal))
  (define c (->color color))
  (when c (send d set-delta-foreground c))
  d)

(define (->color c)
  (cond
    [(not c) #f]
    [(is-a? c color%) c]
    [(and (string? c) (regexp-match #px"^#([0-9a-fA-F]{6})$" c))
     => (lambda (m)
          (define h (cadr m))
          (define (byte-at i) (string->number (substring h i (+ i 2)) 16))
          (make-object color% (byte-at 0) (byte-at 2) (byte-at 4)))]
    [(string? c) (send the-color-database find-color c)]
    [else #f]))

;; ---------------------------------------------------------------------------
;; images
;; ---------------------------------------------------------------------------

(define (load-bitmap src)
  (with-handlers ([(lambda (_) #t) (lambda (_) #f)])
    (cond
      [(bytes? src) (read-bitmap (open-input-bytes src) 'png/alpha)]
      [(file-exists? src) (read-bitmap src)]
      [else #f])))

;; Scale to `scale`, then clamp so the image never overflows the pane.
(define (fit-bitmap bmp scale zoom max-w)
  (define w (send bmp get-width))
  (define h (send bmp get-height))
  (define target (min (* w scale zoom) (max 16.0 (exact->inexact max-w))))
  (define s (/ target w))
  (cond
    [(and (> s 0.98) (< s 1.02)) bmp]
    [else
     (define nw (max 1 (inexact->exact (round (* w s)))))
     (define nh (max 1 (inexact->exact (round (* h s)))))
     (define out (make-bitmap nw nh))
     (define dc (new bitmap-dc% [bitmap out]))
     (send dc set-smoothing 'smoothed)
     (send dc set-scale (/ nw w) (/ nh h))
     (send dc draw-bitmap bmp 0 0)
     (send dc set-bitmap #f)
     out]))

;; ---------------------------------------------------------------------------
;; layout context
;; ---------------------------------------------------------------------------

(struct ctx (txt dc width zoom bands) #:transparent)  ; bands: box of (start end color%)

(define (measure-dc) (new bitmap-dc% [bitmap (make-bitmap 1 1)]))

(define (put! c str delta)
  (define txt (ctx-txt c))
  (define start (send txt last-position))
  (send txt insert str start start #f)
  (send txt change-style delta start (send txt last-position) #f))

(define (put-snip! c snip)
  (define txt (ctx-txt c))
  (send txt insert snip (send txt last-position) 'same #f))

(define (newline! c) (put! c "\n" (make-delta (* base-size (ctx-zoom c)) #f #f #f #f)))

;; ---------------------------------------------------------------------------
;; inline content
;; ---------------------------------------------------------------------------

;; A span becomes either a (text . width) or a (bitmap . width) item.
;; We resolve everything first so the line can be measured before any
;; of it is inserted -- that measurement is what centering needs.
;; `force-bold?` is for headings, which are bold regardless of how
;; their spans are styled.  It has to be applied here rather than
;; afterwards, or the line gets measured in the wrong weight and
;; centered content wraps.
(define (prepare-spans c spans size #:force-bold? [force-bold? #f])
  (define dc (ctx-dc c))
  (for/list ([s (in-list spans)])
    (case (car s)
      [(t)
       (define str (cadr s))
       (define bold? (or force-bold? (caddr s)))
       (define italic? (cadddr s))
       (define tt? (list-ref s 4))
       (define color (list-ref s 5))
       (define f (make-font size bold? italic? tt?))
       (define-values (w h a d) (send dc get-text-extent str f))
       (list 'text str (make-delta size bold? italic? tt? color) w)]
      [(img imgb)
       (define bmp (load-bitmap (cadr s)))
       (cond
         [bmp
          (define fitted (fit-bitmap bmp (caddr s) (ctx-zoom c) (ctx-width c)))
          (list 'bitmap fitted (send fitted get-width))]
         [else
          (define msg (format "[missing image: ~a]"
                              (if (bytes? (cadr s)) "<inline>" (cadr s))))
          (define f (make-font size #f #t #f))
          (define-values (w h a d) (send dc get-text-extent msg f))
          (list 'text msg (make-delta size #f #t #f "red") w)])]
      [(br) (list 'text "\n" (make-delta size #f #f #f #f) 0)]
      [else (list 'text "" (make-delta size #f #f #f #f) 0)])))

(define (items-width items)
  (for/sum ([i (in-list items)]) (list-ref i (if (eq? 'bitmap (car i)) 2 3))))

(define (insert-items! c items align)
  (define total (items-width items))
  (define avail (ctx-width c))
  ;; Only center content that fits on one line; once the text% wraps,
  ;; a leading spacer would indent just the first line.  The slack
  ;; covers small differences between our measuring dc and the
  ;; editor's own metrics.
  (when (and (memq align '(center right)) (<= total (- avail 8)))
    (define pad (if (eq? align 'center) (/ (- avail total) 2) (- avail total)))
    (when (> pad 1) (put-snip! c (new spacer-snip% [spacer-width pad]))))
  (for ([i (in-list items)])
    (case (car i)
      [(text) (put! c (cadr i) (caddr i))]
      [(bitmap) (put-snip! c (make-object image-snip% (cadr i)))])))

;; ---------------------------------------------------------------------------
;; blocks
;; ---------------------------------------------------------------------------

;; Every block ends with exactly one newline unless `break?` is #f;
;; blank-line separation is the caller's business.
(define (insert-block! c b #:break? [break? #t])
  (define zoom (ctx-zoom c))
  (case (car b)
    [(heading)
     (define depth (cadr b))
     (define align (caddr b))
     (define scale (if (< depth (vector-length heading-scales))
                       (vector-ref heading-scales depth)
                       1.0))
     (define size (* base-size zoom scale))
     (insert-items! c (prepare-spans c (cdddr b) size #:force-bold? #t) align)
     (when break? (newline! c))]

    [(para)
     (define size (* base-size zoom))
     (insert-items! c (prepare-spans c (cddr b) size) (cadr b))
     (when break? (newline! c))]

    [(nested)
     (define bgc (->color (cadr b)))
     (define fgc (->color (caddr b)))
     (define txt (ctx-txt c))
     (define start (send txt last-position))
     (insert-blocks! c (cdddr b))
     (define end (send txt last-position))
     (when bgc
       (set-box! (ctx-bands c) (cons (list start end bgc) (unbox (ctx-bands c)))))
     (when fgc
       (define d (new style-delta%))
       (send d set-delta-foreground fgc)
       (send txt change-style d start end #f))]

    [(items)
     (define ordered? (eq? 'ordered (cadr b)))
     (for ([item (in-list (cddr b))] [n (in-naturals 1)])
       (put! c (if ordered? (format "~a. " n) "• ")
             (make-delta (* base-size zoom) #f #f #f #f))
       (insert-blocks! c item #:blank-lines? #f))]

    [(table)
     (for ([row (in-list (cdr b))])
       (define cells (filter (lambda (x) (not (eq? 'cont x))) row))
       (for ([cell (in-list cells)] [n (in-naturals)])
         (unless (zero? n)
           (put! c "  " (make-delta (* base-size zoom) #f #f #t #f)))
         (for ([blk (in-list cell)] [k (in-naturals)])
           (insert-block! c blk #:break? (and (< k (sub1 (length cell))) #t))))
       (newline! c))]

    [else (void)]))

(define (insert-blocks! c blocks #:blank-lines? [blank-lines? #t])
  (for ([b (in-list blocks)] [n (in-naturals)])
    (when (and blank-lines? (positive? n)) (newline! c))
    (insert-block! c b)))

;; ---------------------------------------------------------------------------
;; entry points
;; ---------------------------------------------------------------------------

(define (with-fresh-text txt thunk)
  (send txt begin-edit-sequence)
  (send txt lock #f)
  (when (is-a? txt preview-text%) (send txt clear-bands!))
  (send txt erase)
  (send txt set-styles-sticky #f)
  (thunk)
  (send txt set-position 0 0)
  (send txt lock #t)
  (send txt end-edit-sequence))

;; `width` is the usable pane width in pixels; re-call on resize.
(define (render-display-list! txt blocks width [zoom 1.0])
  (define bands (box '()))
  (define c (ctx txt (measure-dc) (max 80 (- width 24)) zoom bands))
  (with-fresh-text txt (lambda () (insert-blocks! c blocks)))
  (when (is-a? txt preview-text%)
    ;; Bands are painted in list order, so outer regions have to come
    ;; first and let nested ones paint on top -- an outer band fully
    ;; contains its children's ranges, so the reverse order hides them
    ;; completely.  A nested block pushes its own band only after
    ;; recursing into its children, which means the box already holds
    ;; them outermost-first; reversing it was exactly wrong.
    (send txt set-bands! (unbox bands))
    (send txt invalidate-bitmap-cache)))

(define (render-error! txt message log [zoom 1.0])
  (define c (ctx txt (measure-dc) 400 zoom (box '())))
  (with-fresh-text
   txt
   (lambda ()
     (put! c "Preview failed\n\n" (make-delta (* base-size zoom 1.3) #t #f #f "firebrick"))
     (put! c message (make-delta (* base-size zoom) #f #f #t "firebrick"))
     (put! c "\n" (make-delta (* base-size zoom) #f #f #f #f))
     (when (and log (not (string=? log "")))
       (put! c "\nBuild output\n" (make-delta (* base-size zoom) #t #f #f "gray"))
       (put! c log (make-delta (* base-size zoom 0.9) #f #f #t "gray"))))))

(define (render-message! txt message [zoom 1.0])
  (define c (ctx txt (measure-dc) 400 zoom (box '())))
  (with-fresh-text
   txt
   (lambda ()
     (put! c message (make-delta (* base-size zoom) #f #t #f "gray")))))
