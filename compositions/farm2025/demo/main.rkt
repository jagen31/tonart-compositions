#lang racket



(require (prefix-in toc: "toccat-decomp.rkt")
         (prefix-in f2025: "proposal.rkt")

         











         #;(!! opening opening2)
         #;(!! bach-d (note->midi) (dilate 1/4))
         (prefix-in f2024: "farm-demo-2024.rkt")
         









         

         pict/convert 
         "toccat.rkt"
         (prefix-in gui: racket/gui)
         slideshow/slides-to-picts
         "script.rkt"

         (except-in tonart string) (prefix-in 2htdp: 2htdp/image) (prefix-in 2htdp: 2htdp/universe)
         (prefix-in pict: pict)
         tonart/private/common-practice/transform
         tonart/private/common-practice/lib
         tonart/private/electronic/lib
         art/coordinate/switch
         "organ-config.rkt"
         "performance-demo.rkt"
         (for-syntax syntax/parse racket/list racket/string syntax/to-string fmt))

(define-art-rewriter trumpet-blast
  (syntax-parser
    [(_ [note1 ...] [note2 ...])
     (qq-art this-syntax
             (context
              (seq (ix-- (note note1 ...) (note note1 ...) (note note1 ...) (note note2 ...)))
              (rhythm 1/4 1/4 1/4 2)
              (apply-rhythm)))]))

(define-art opening2
  (-- [4]
      [4 (voice@ (lower) (trumpet-blast [g 0 4] [a 0 4]))
          (voice@ (pedal) (trumpet-blast [c 0 2] [f 0 2]))]
      [4 (voice@ (lower) (trumpet-blast [c 0 5] [f 0 5]))
         (voice@ (lower) (trumpet-blast [c 0 4] [f 0 4]))
         (voice@ (pedal) (trumpet-blast [c 0 2] [f 0 2]))])
  (voice@ (lower) (constant-structure) (structure-notes [M])
          (chord->notes/simple 5))
  )
(define-art opening
  (voice@ [lower] (trumpet-blast [c 0 4] [f 0 4])))

(define-for-syntax command-center-state 
  (box (run-art-expr #'(context
                        (quoted-rewrite (^o->note))
                        (quoted-rewrite (delete seq)))
                      '())))

(define-for-syntax (get-command-center-state)
  (sort (unbox command-center-state) < #:key expr-single-index))

(define the-slide-frame
  (new gui:frame%
    [label "Window"]
    [width 1100] 
    [height 820]))

(define-art-rewriter post-a
  (λ (stx)
    #'(context (key a 0 minor) (note->^o) (chord->chord^) (delete key) (dilate 1/6))))


(define-for-syntax organ-keymap
  (run-art-expr
    #`(context

      ;; "drumkit"
      (name@ up (music (i@ [0 1] (sound snare))))
      (name@ down (music (i@ [0 1] (volume 30) (sound kick))))
      (name@ left (music (i@ [0 1] (sound cymbal))))

      ;; I/V pedal tones
      (name@ o
        (music
          (i@ [0 8] (^o 1 3) (manual P))))
      (name@ p
        (music 
          (i@ [0 8] (^o 5 3) (manual P))))

      ;; I/V arpeggios 
      (name@ k
        (music
          (i@ [0 8] (manual II) (seq (^s 1 3 5 8)) (urhy* 2)) (octave 4) (^->^o) (delete octave) (dilate 1/16)))
      (name@ l
        (music 
          (i@ [0 8] (manual II) (seq (^s -2 [0 1] 2 5)) (urhy* 2)) (octave 4) (^->^o) (delete octave) (dilate 1/16)))

      (name@ h (music f2024:harmony (key a 0 minor) (chord->chord^) (delete key) (dilate 1/2)))

      (name@ t (rkt (!>
        (ix@ 2 (name@ note-compiler
               (delete)
               (quoted-rewrite (note->tone)))))))
      (name@ m (rkt (!>
        (ix@ 2 (name@ note-compiler
               (delete)
               (quoted-rewrite (note->midi)))))))

      (name@ e (rkt 
        (!<
          (name@ default-reg (delete)))
        (!!>
          (ix@ 1 (name@ echo
              (delete)
              (quoted-rewrite (seq (ix-- (music (manual I) (hole)) 
                                  (music (manual II) (hole)))))
              (quoted-rewrite (rewrite-in-seq (rewrite-in-music (fill-holes* note))))
              (quoted-rewrite (inline-music-seq)))))))

      (name@ r (rkt 
        (!!> (ix@ 1 (name@ echo (delete))))
        (!< (name@ default-reg (manual I)))))

      (name@ v (music toc:main-theme (key a 0 minor) (note->^o) (dilate 1/4) (delete key)))
      (name@ b (music toc:bounce (key d 0 minor) (note->^o) (dilate 1/4) (delete key)))

      (name@ a 
        (music 
          (voice@ [soprano] (manual I))
          (voice@ [alto] (manual II))
          f2025:theme1 (key a 0 minor) (note->^o) (dilate 1/4) (delete key)))
      (name@ s 
        (music 
          (manual P)
          f2024:theme (key a 0 minor) (note->^o) (transpose-octave -1) 
          (run-transpose-octave) (dilate 1/4) (delete key)))

      (name@ r
             (music
              (voice@ [upper]
                      (seq (^s 1 [1 -1] 1 [1 -1] 1 [1 -1] 1 [1 -1] 1 [1 -1]))
                      (rhythm 1 1 1 1 0.5 0.5 0.5 0.5 1 1)
                      (apply-rhythm)
                      (octave 4))
              (^->^o)
              (dilate 1/4)))

      (name@ d 
        (music 
          (voice@ [upper] f2024:theme-a) (manual I) (key a 0 minor) (note->^o) (dilate 1/2) (delete key)))

      (name@ c 
        (music 
          (voice@ [upper] f2024:theme-b) (manual I) (key a 0 minor) (note->^o) (dilate 1/2) (delete key)))

      (name@ f
        (music 
          f2024:theme-c (manual I) (key a 0 minor) (note->^o) (dilate 1/2) (delete key)))

      (name@ ! (rkt (!< (toggle-invert))))
      
      (name@ u (rkt (!< (transpose-key [4 P]))))
      (name@ i (rkt (!< (transpose-key [5 P]))))
      (name@ q (rkt (!< (prev-mode))))
      (name@ w (rkt (!< (next-mode))))
      (name@ |8| (rkt (!< (transpose-key [1 P] major))))
      (name@ |9| (rkt (!< (transpose-key [1 P] minor))))

      (name@ |1| (rkt (set-slide! 0) (gui:send the-slide-frame refresh)))
      (name@ |2| (rkt (set-slide! 1) (gui:send the-slide-frame refresh)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; ORGAN REGISTRATION BINDINGS
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; general cancel
      (name@ _ (music (@ [(instant 0) (switch #t)] (registration))))

      ;; some presets
      (name@ |[| (music (@ [(instant 0) (switch #t)] reg)))
      (name@ |]| (music (@ [(instant 0) (switch #t)] reg2)))
      (name@ |\| (music (@ [(instant 0) (switch #t)] reg3)))
      (name@ |'| (music (@ [(instant 0) (switch #t)] reg4)))


      ;; crescendos/decrescendos
      
      ;;;; through the presets
      ;;;;;; crescendo
      (name@ |}| 
        (music
          (@ [(instant 0) (switch #t)] reg)
          (@ [(instant 4) (switch #t)] reg2)
          (@ [(instant 8) (switch #t)] reg3)
          (@ [(instant 12) (switch #t)] reg4)))

      ;;;;;; decrescendo
      (name@ |{|   
        (music
          (@ [(instant 0) (switch #t)] reg4)
          (@ [(instant 4) (switch #t)] reg3)
          (@ [(instant 8) (switch #t)] reg2)
          (@ [(instant 12) (switch #t)] reg)))

      ;;;; through the flutes
      ;;;;;; crescendo
      (name@ + 
        (music
          (@ [(instant 0) (switch #t)] (registration+ [I flute] [II cor-de-nuit] [III bourdon] [Aux III/II]))
          (@ [(instant 4) (switch #t)] (registration+ [I harm-flute] [III flute]))
          (@ [(instant 8) (switch #t)] (registration+ [I flute-4] [III flute-4]))
          (@ [(instant 12) (switch #t)] (registration+ [I flute-16] [II flute-2] [Aux II/I]))))

      ;;;;;; decrescendo
      (name@ -
        (music

          (@ [(instant 4) (switch #t)] (registration+  [Aux II/I III/II]))

          (@ [(instant 16) (switch #f)] (registration+ [I flute] [II cor-de-nuit] [III bourdon] [Aux III/II]))
          (@ [(instant 12) (switch #f)] (registration+ [I harm-flute] [III flute]))
          (@ [(instant 8) (switch #f)] (registration+  [I flute-16] [III flute-4] [I flute-4] [Aux II/I]))
          (@ [(instant 4) (switch #f)] (registration+  [II flute-2]))))

      ;;;; through the reeds
      ;;;;;; crescendo
      (name@ >
        (music
          (@ [(instant 0) (switch #t)] (registration+ [II clarinet] [III oboe] [Aux III/I]))
          (@ [(instant 4) (switch #t)] (registration+ [II krumhorn] [III trumpet]))
          (@ [(instant 8) (switch #t)] (registration+ [I trumpet] [Aux III/II II/I]))
          (@ [(instant 12) (switch #t)] (registration+ [I clarion] [III clarion bassoon]))))

      ;;;;;; decrescendo
      (name@ <
        (music
          (@ [(instant 16) (switch #f)] (registration+ [II clarinet] [III oboe] #;[Aux III/I]))
          (@ [(instant 12) (switch #f)] (registration+ [II krumhorn] [III trumpet]))
          (@ [(instant 8) (switch #f)] (registration+ [I trumpet] #;[Aux III/II II/I]))
          (@ [(instant 4) (switch #f)] (registration+ [I clarion] [III clarion bassoon]))))

      (name@ z
             (music (@ [(instant 0) (switch #t)] (registration+ [P bombarde]))))
      (name@ x
             (music (@ [(instant 0) (switch #f)] (registration+ [P bombarde]))))
      )
      '()))

(define my-on-char (box #f))
(define the-image (box 2htdp:empty-image))

(define the-state-image (box 2htdp:empty-image))
(define the-post-state-image (box 2htdp:empty-image))
(define the-command-center-state-image (box 2htdp:empty-image))


(define the-frame
  (new gui:frame%
    [label "Commands"]
    [width 800]
    [height 1100]))

(define the-state-frame
  (new gui:frame%
    [label "Context"]
    [width 900] 
    [height 300]))

(define the-post-state-frame
  (new gui:frame%
    [label "Compiler Rewrites"]
    [width 450] 
    [height 800]))

(define the-command-center-state-frame
  (new gui:frame%
    [label "Display Rewrites"]
    [width 450] 
    [height 800]))


(define-syntax (compile-keymap stx)

  (cond [state-dirty

         
         
         (define results
           (for/list ([expr (run-art-exprs organ-keymap '() (unbox state))])

             (define dat (syntax->datum (expr-single-name expr)))
             (list (if (number? dat) (string->symbol (number->string dat)) dat) expr)))

         (define-values (music-exprs- rkt-exprs-) 
           (partition (λ (expr) (free-identifier=? (car (syntax->list (cadr expr))) #'music)) results))

         (define/syntax-parse ([the-key ({~literal music} music-exprs ...)] ...) music-exprs-)
         (define/syntax-parse ([rkt-key ({~literal rkt} rkt-exprs ...)] ...) rkt-exprs-)
         
         #`(begin
             (2htdp:save-image (2htdp:scale 2 #,(draw-logo (run-art-exprs (unbox state) '())))
                               "images/logo.png")
             (set-box! the-slides (get-slides-as-picts "slides.rkt" 1100 800 #f))
             (refresh-slide!)
             
             (define-syntax _ (set-box! state-dirty #f))
             
             (set-box! my-on-char
                       (λ (event)
                         (define code (gui:send event get-key-code)) 
                         (define sym (if (symbol? code) code (string->symbol (string code))))
                         (match sym
                           ['the-key (!! music-exprs ...) (-> 0)] ... 
                           ['rkt-key (eval-syntax #'(begin rkt-exprs ...))] ...
                           [_ (void)])))
             (set-box! the-image 
                       (2htdp:freeze
                        (dr (namespace #,@(unbox state) #,@organ-keymap
                                       (rewrite-in-music 
                                        #,@(get-command-center-state)
                                        (run-rewriters)
                                        (delete direction time-sig divisions))))))
             (set-box! the-state-image (realize (staff-realizer [800 200] {}) #,@(unbox state)))
             (set-box! the-post-state-image (dr #,@(get-post-state)))
             (set-box! the-command-center-state-image (dr #,@(get-command-center-state)))
             (gui:send the-frame refresh)
             (gui:send the-state-frame refresh)
             (gui:send the-slide-frame refresh)
             (gui:send the-post-state-frame refresh)
             (gui:send the-command-center-state-frame refresh))]
        [else (void)]))

(define-syntax (!< stx)
  (syntax-parse stx
    [(_ expr ...)
     (set-box! state-dirty (box #t))
     #'(begin  (!<- expr ...) (compile-keymap))]))

(define-syntax (!> stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(begin (!-> expr ...) (compile-keymap))]))

(define-syntax (!2> stx)
  (set-box! state-dirty (box #t))
  (syntax-parse stx
    [(_ expr ...)
     (set-box! command-center-state (run-art-exprs (append (get-command-center-state) (syntax->list #'(expr ...))) '()))
     (set-box! state-dirty (box #t))

     #'(compile-keymap)]))

(define-syntax (!!> stx)
  (set-box! state-dirty (box #t))
  (syntax-parse stx
    [(_ expr ...)
     (set-box! post-state (run-art-exprs (append (get-post-state) (syntax->list #'(expr ...))) '()))
     (set-box! command-center-state (run-art-exprs (append (get-command-center-state) (syntax->list #'(expr ...))) '()))
     (set-box! state-dirty (box #t))

     #'(compile-keymap)]))

(compile-keymap)

(define-syntax (print-state stx)
  (println (map un-@ (unbox state)))
  #'(void))

(define my-canvas%
  (class gui:canvas%
    (super-new)
    
    (define/override (on-char event)
      ((unbox my-on-char) event))))


(define canvas
  (new my-canvas% [parent the-frame]
                  [style (list 'hscroll 'vscroll)]
                  [paint-callback
                   (lambda (canvas dc)
                     (gui:send dc draw-bitmap (pict:pict->bitmap (unbox the-image)) 0 0))]))
  
(gui:send canvas show-scrollbars #t #t)

(gui:send canvas init-auto-scrollbars 2000
  2000
  0
  0)

(define state-canvas
  (new my-canvas% [parent the-state-frame]
                  [style (list 'hscroll 'vscroll)]
                  [paint-callback
                   (lambda (canvas dc)
                     (gui:send dc draw-bitmap (pict:pict->bitmap (unbox the-state-image)) 0 0))]))

(define post-state-canvas
  (new my-canvas% [parent the-post-state-frame]
                  [style (list 'hscroll 'vscroll)]
                  [paint-callback
                   (lambda (canvas dc)
                     (gui:send dc draw-bitmap (pict:pict->bitmap (unbox the-post-state-image)) 0 0))]))

(define command-center-state-canvas
  (new my-canvas% [parent the-command-center-state-frame]
                  [style (list 'hscroll 'vscroll)]
                  [paint-callback
                   (lambda (canvas dc)
                     (gui:send dc draw-bitmap (pict:pict->bitmap (unbox the-command-center-state-image)) 0 0))]))

(define slide-canvas
  (new my-canvas% [parent the-slide-frame]
                  [style (list 'hscroll 'vscroll)]
                  [paint-callback
                   (lambda (canvas dc)
                     (gui:send dc draw-bitmap (pict:pict->bitmap (unbox the-slide)) 0 0))]))

(gui:send state-canvas show-scrollbars #t #t)
(gui:send post-state-canvas show-scrollbars #t #t)
(gui:send command-center-state-canvas show-scrollbars #t #t)

(gui:send state-canvas init-auto-scrollbars 2000
  2000
  0
  0)
(gui:send post-state-canvas init-auto-scrollbars 2000
  2000
  0
  0)
(gui:send command-center-state-canvas init-auto-scrollbars 2000
  2000
  0
  0)
(gui:send the-frame move 0 0)
(gui:send the-state-frame move 800 0)
(gui:send the-command-center-state-frame move 800 500)
(gui:send the-post-state-frame move 1250 500)
(gui:send the-slide-frame move 0 500)

#;(void (thread (lambda () (gui:send the-frame show #t))))
(void (thread (lambda () (gui:send the-state-frame show #t))))
(void (thread (lambda () (gui:send the-post-state-frame show #t))))
#;(void (thread (lambda () (gui:send the-command-center-state-frame show #t))))
(void (thread (lambda () (gui:send the-slide-frame show #t))))

#;(void (thread (lambda () (let refresher () (sleep 0.5)
                           (eval-syntax #'(compile-keymap))
                           (gui:send the-slide-frame refresh)                       
                           (refresher)))))