#lang racket/base

;; DrRacket plugin: a realizer preview pane on the right of the
;; definitions window.  Buttons run the scribble or strudel realizer
;; over the edited module's `program` art.
;;
;; The pane is inserted by overriding
;; `get-definitions/interactions-panel-parent`, which is the documented
;; hook for adding containers to the DrRacket frame -- Check Syntax's
;; error report and the debugger's stack view both go in the same way.
;; We return a child of the super's panel, having first slipped a
;; horizontal dragable panel in between, so definitions and preview sit
;; side by side with a draggable divider.

(require racket/class
         racket/unit
         racket/gui/base
         framework
         drracket/tool
         "preview-panel.rkt")

(provide tool@)

(preferences:set-default 'realizer-preview:visible? #f boolean?)
(preferences:set-default 'realizer-preview:percentage 3/5
                         (lambda (x) (and (real? x) (< 0 x 1))))

;; `panel:dragable-mixin` multiplies percentages by pixel counts and
;; `container-redraw` then insists the results be *exact* integers.  A
;; flonum percentage silently poisons the whole layout: place-children
;; returns `(0 0 1033.0 1031)`, the panel rejects it, and the pane
;; never appears.  That is why DrRacket's own percentages are 1/2 and
;; 1/5 rather than 0.5 and 0.2.  Round to hundredths so the stored
;; value stays readable and `(- 1 p)` sums to exactly 1.
(define (exact-percentage x)
  (if (and (real? x) (< 0 x 1))
      (/ (round (* 100 (inexact->exact x))) 100)
      3/5))

(define (show-label visible?)
  (if visible? "Hide Realizer Preview" "Show Realizer Preview"))

(define (preview-frame-mixin super%)
  (class super%
    (inherit get-definitions-text set-show-menu-sort-key)

    ;; These have to be plain fields defined before `super-new`: the
    ;; superclass constructor calls
    ;; `get-definitions/interactions-panel-parent` on the way up.
    (define split-panel #f)
    (define editor-side #f)
    (define preview #f)
    (define menu-item #f)
    (define visible? (preferences:get 'realizer-preview:visible?))

    (super-new)

    (define/override (get-definitions/interactions-panel-parent)
      (define outer (super get-definitions/interactions-panel-parent))
      (set! split-panel (new panel:horizontal-dragable% [parent outer]))
      (set! editor-side (new vertical-panel% [parent split-panel]))
      (set! preview (new preview-panel%
                         [parent split-panel]
                         [get-source (lambda () (current-source))]))
      (apply-visibility)
      editor-side)

    ;; What the realizer should run on: the current tab's file, its
    ;; text, and whether the buffer has unsaved edits.
    (define/private (current-source)
      (define defs (get-definitions-text))
      (and defs
           (list (send defs get-filename)
                 (send defs get-text)
                 (and (send defs is-modified?) #t))))

    (define/private (apply-visibility)
      (when split-panel
        (send split-panel begin-container-sequence)
        (send split-panel change-children
              (lambda (_) (if visible? (list editor-side preview) (list editor-side))))
        (when visible?
          (define p (exact-percentage (preferences:get 'realizer-preview:percentage)))
          (with-handlers ([exn:fail? void])
            (send split-panel set-percentages (list p (- 1 p))))
          (send preview ensure-rendered))
        (send split-panel end-container-sequence))
      (when menu-item
        (send menu-item set-label (show-label visible?))))

    (define/private (toggle-preview)
      ;; remember where the user left the divider
      (when (and visible? split-panel)
        (define ps (send split-panel get-percentages))
        (when (= 2 (length ps))
          (preferences:set 'realizer-preview:percentage (exact-percentage (car ps)))))
      (set! visible? (not visible?))
      (preferences:set 'realizer-preview:visible? visible?)
      (apply-visibility))

    (define/override (add-show-menu-items show-menu)
      (super add-show-menu-items show-menu)
      (set! menu-item
            (new menu:can-restore-menu-item%
                 [label (show-label visible?)]
                 [parent show-menu]
                 [callback (lambda (i e) (toggle-preview))]
                 [shortcut #\r]
                 [shortcut-prefix (cons 'shift (get-default-shortcut-prefix))]))
      (set-show-menu-sort-key menu-item 106))

    (define/augment (on-close)
      (when preview (send preview shutdown))
      (inner (void) on-close))))

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (drracket:get/extend:extend-unit-frame preview-frame-mixin)))
