#lang at-exp racket

(require tonart
         ;; cursor lives in cool — not re-exported by tonart itself.
         ;; we need its identifier in scope so the realizer's
         ;; {~literal cursor} pattern can free-identifier=? match
         ;; cursors placed in the score.
         (only-in tonart/private/common-practice/cool cursor)
         (for-syntax syntax/parse racket/string racket/format racket/match racket/dict racket/list racket/math racket/set
                     art/coordinate/index))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-for-syntax (sa . str)
  (apply string-append (map ~a str)))

;; an image to render above the staff at a particular time.
;; the image is sized to roughly the height in staff-spaces given by `image-scale`.
(define-art-object (image [filename]))

;; a forced system break.  the realizer collects all line-break
;; instances (regardless of voice) and emits \break in every voice
;; after the event whose end >= the break's start time.
(define-art-object (line-break []))

;; (pickup d): tells the realizer to emit \partial d at the start of
;; every voice and shift the bar line so the music's t=0 is the upbeat
;; instead of beat 1.  d is measured in quarter notes — e.g. 1 = a
;; single quarter pickup, 1/2 = eighth pickup, 3/2 = dotted quarter.
(define-art-object (pickup [duration]))

;; controls the top-to-bottom order of staves in the rendered score.
;; voices listed here render first in the given order; any voices not
;; listed retain their natural (collection) order after the listed
;; ones.  the realizer takes the first voice-order it encounters.
(define-art-object (voice-order [voices]))

;; one syllable of lyric text.  the realizer collects all lyric
;; objects per voice (sorted by `index` coordinate, set up by an
;; enclosing `(lyrics …)` / `(ix-- …)`) and emits them as a
;; `\new Lyrics \lyricsto "<voice>" { … }` block under that voice's
;; staff.  one lyric → one note in the voice it sits in.
(define-art-object (lyric [word]))

;; (lyrics "w1" "w2" …) sugar — assigns each word a fresh `index`
;; coordinate via `ix--` so the realizer can sort them in source
;; order even after rewriters shuffle the context.
(define-art-rewriter lyrics
  (λ (stx)
    (syntax-parse stx
      [(_ w ...)
       (qq-art stx (ix-- (lyric w) ...))])))

;; one figured-bass marking — the contents go inside lilypond's
;; `<…>` so e.g. `(figure "6")` → `<6>`, `(figure "6 4")` → `<6 4>`,
;; `(figure "")` → `<>` (empty / no figure for this note).  Each
;; (figure …) consumes one slot in the bass voice.
(define-art-object (figure [text]))

;; (hint "TEXT") — a free-text annotation that floats above the
;; top staff at its placed time.  Renders through lilypond's
;; score-level `\mark \markup { "TEXT" }` plumbing — same machinery
;; as `(cursor)`, but with arbitrary text instead of a fixed segno
;; glyph.  Use to leave the player a quick visual cue at a
;; specific beat.  Voiceless `(hint …)`s only render on the first
;; voice — `\mark` is score-level, so emitting one per voice would
;; stack identical marks at the same spot.
(define-art-object (hint [text]))

;; (tuplet n d) — wrap the notes inside this time interval in a
;; lilypond `\tuplet n/d { … }` bracket.  Each contained note is
;; re-rendered at `actual-dur × n / d` so its lily duration token
;; comes out as the "normal" value (e.g. a 1/3-quarter inside
;; `(tuplet 3 2)` renders as `8`, an eighth).  Without this the
;; duration splitter resolves a 1/3-quarter to `16~64~64` tied
;; subdivisions, which is technically the right total length but
;; reads as garbage on the page.  Place the (tuplet …) at the
;; exact start/end of the run of notes you want bracketed; tuplets
;; that don't cleanly contain their notes are silently ignored.
;; `(triplet)` is the common 3:2 case.
(define-art-object (tuplet [num denom]))

(define-art-rewriter triplet
  (λ (stx)
    (syntax-parse stx
      [(_) (qq-art stx (tuplet 3 2))])))

;; (figured-bass "f1" "f2" …) — sugar for one figure per bass note
;; in source order via `ix--`.  Use "" for notes that should display
;; no figure (figure inheritance + blanks are common in real figured
;; bass).  Attach inside a `(voice@ [bass-voice] …)`.
(define-art-rewriter figured-bass
  (λ (stx)
    (syntax-parse stx
      [(_ f ...)
       (qq-art stx (ix-- (figure f) ...))])))

(define-for-syntax image-scale 6)
;; how much horizontal extent (in staff-spaces) lilypond is told the
;; image occupies — sized to roughly match the rendered dancer's
;; actual visual width (~3 staff-spaces at image-scale=6, EPS aspect
;; ~100:186) plus a margin.  this is plumbed through TextScript's
;; `extra-spacing-width` override below so it reaches the spacing
;; engine; bump if dancers crash into each other, drop if the music
;; gets pushed onto too many systems.
(define-for-syntax image-h-extent 4)

(define-for-syntax (image-markup filename
                                 [scale image-scale]
                                 [h-extent image-h-extent])
  ;; pick \epsfile or \image based on extension; both share the same
  ;; #X #size #"file" calling convention.  We wrap with \with-dimensions
  ;; so lilypond reserves a (configurable) horizontal footprint and the
  ;; image's vertical footprint, and \center-align keeps it centered
  ;; above the note column.  Bumping h-extent forces the spacing engine
  ;; to allocate more horizontal room per image without changing the
  ;; rendered dancer size.
  ;;
  ;; \override #'(background-color . #f) keeps transparent regions
  ;; transparent (lilypond otherwise fills them with white).  This works
  ;; reliably for EPS in the default PostScript backend; for PNG it only
  ;; works in the Cairo / SVG backends.
  (define ext (let ([d (regexp-match #rx"\\.[^./\\]+$" filename)])
                (if d (string-downcase (car d)) "")))
  (define cmd (if (equal? ext ".eps") "\\epsfile" "\\image"))
  (define h-half (/ h-extent 2))
  (sa "^\\markup { \\override #'(background-color . #f) "
      "\\with-dimensions #'(" (- h-half) " . " h-half ") #'(0 . " scale ") \\center-align "
      cmd " #Y #" scale " #\"" filename "\" }"))

;; tonart octave 4 = middle C = lilypond c'
(define-for-syntax (pitch->lily p a o)
  (define base (symbol->string p))
  (define acc (match a
                [0  ""]
                [1  "is"]
                [-1 "es"]
                [2  "isis"]
                [-2 "eses"]
                [_  ""]))
  (define oct-str
    (cond
      [(>= o 4) (make-string (- o 3) #\')]
      [(< o 3)  (make-string (- 3 o) #\,)]
      [else     ""]))
  (string-append base acc oct-str))

;; duration in quarter notes -> (list-of lilypond-duration-string)
;; if the duration cannot be expressed as a single (possibly dotted) value,
;; split it into a sum of values that lilypond can tie together.
(define-for-syntax basic-durations
  ;; (quarter-note-value . lilypond-string)
  '((4    . "1")    (3    . "2.")
    (2    . "2")    (3/2  . "4.")
    (1    . "4")    (3/4  . "8.")
    (1/2  . "8")    (3/8  . "16.")
    (1/4  . "16")   (3/16 . "32.")
    (1/8  . "32")   (1/16 . "64")))

(define-for-syntax (duration->lily dur)
  (let loop ([remaining (inexact->exact dur)] [acc '()])
    (cond
      [(<= remaining 0) (reverse acc)]
      [else
       (define match
         (for/or ([d (in-list basic-durations)])
           (and (<= (car d) remaining) d)))
       (if match
           (loop (- remaining (car match)) (cons (cdr match) acc))
           ;; fallback: emit a 64th note and stop to avoid infinite loops
           (reverse (cons "64" acc)))])))

;; bar-aware splitter: splits `dur` into pieces that never cross a
;; barline.  `start` is the absolute position (in quarter notes from
;; the start of the music) where this duration begins; `pickup-val`
;; (or 0) is the music's anacrusis offset that shifts where bar
;; lines fall; `bar-len` is the bar length in quarter notes derived
;; from the active time-sig.  this is what gives us rhythmically
;; natural splits like `a2.~ a2.` (two dotted halves in 3/4) rather
;; than the greedy `a1~ a2` (whole + half) you'd get from a purely
;; size-based split.  if `bar-len` is #f we fall back to the plain
;; greedy splitter.
(define-for-syntax (duration->lily/bar dur start pickup-val bar-len)
  (cond
    [(not bar-len) (duration->lily dur)]
    [else
     (let* ([exact-dur (inexact->exact dur)]
            [exact-bar (inexact->exact bar-len)]
            [shifted   (- (inexact->exact start)
                          (inexact->exact (or pickup-val 0)))]
            [bar-pos   (- shifted
                          (* exact-bar (floor (/ shifted exact-bar))))])
       (let loop ([remaining exact-dur]
                  [pos bar-pos]
                  [acc '()])
         (cond
           [(<= remaining 0) (reverse acc)]
           [else
            (define to-bar-end (- exact-bar pos))
            (define chunk-max (min remaining to-bar-end))
            (define pieces (duration->lily chunk-max))
            (loop (- remaining chunk-max)
                  0
                  (append (reverse pieces) acc))])))]))

(define-for-syntax (note->lily-tokens p a o dur start pickup-val bar-len)
  (chord->lily-tokens (list (pitch->lily p a o)) dur start pickup-val bar-len))

;; emit a list of pitch strings as a single lilypond chord (or note if
;; only one pitch).  splits long durations into tied pieces just like
;; note->lily-tokens does for single notes.
(define-for-syntax (chord->lily-tokens pitches dur start pickup-val bar-len)
  (define durs (duration->lily/bar dur start pickup-val bar-len))
  (define body
    (cond
      [(= (length pitches) 1) (car pitches)]
      [else (string-append "<" (string-join pitches " ") ">")]))
  (cond
    [(null? durs) (list (string-append body "4"))]
    [(= (length durs) 1) (list (string-append body (car durs)))]
    [else
     (for/list ([d (in-list durs)] [i (in-naturals)])
       (define tie (if (< i (sub1 (length durs))) "~" ""))
       (string-append body d tie))]))

(define-for-syntax (rest->lily-tokens dur start pickup-val bar-len)
  (for/list ([d (in-list (duration->lily/bar dur start pickup-val bar-len))])
    (string-append "r" d)))

(define-for-syntax (skip->lily-tokens dur start pickup-val bar-len)
  (for/list ([d (in-list (duration->lily/bar dur start pickup-val bar-len))])
    (string-append "s" d)))

(define-for-syntax (voice-name expr)
  (define v (expr-voice expr))
  (cond
    [(and (cons? v) (cons? (car v)))
     ;; nested: take the innermost voice id
     (syntax->datum (car v))]
    [(cons? v) (syntax->datum (car v))]
    [else 'main]))

(define-art-realizer music-lilypond-realizer
  (λ (stx)
    (syntax-parse stx
      [(_ (~alt (~optional (~seq #:beat beat:number) #:defaults ([beat #'1]))
                (~optional (~seq #:title title:str) #:defaults ([title #'"tonart"])))
          ...)
       (define beat-val (syntax-e #'beat))
       (define title-val (syntax-e #'title))
       ;; (pickup d) art-object → emit \partial <d> at the start of
       ;; each voice and treat the music's t=0 as the upbeat instead
       ;; of beat 1.  d is in quarter notes (e.g. 1 = quarter,
       ;; 1/2 = eighth, 3/2 = dotted quarter).  First pickup wins.
       (define pickup-val
         (for/or ([expr (in-list (lookup-ctxt))])
           (syntax-parse expr
             [({~literal pickup} d:number) (syntax-e #'d)]
             [_ #f])))
       ;; collect any (voice-order [v1 v2 ...]) art-objects placed in
       ;; the score.  the first one we find wins; any voices it lists
       ;; render first in the given order.
       (define ordered-voice-syms
         (or (for/or ([expr (in-list (lookup-ctxt))])
               (syntax-parse expr
                 [({~literal voice-order} (v:id ...))
                  (map syntax-e (syntax->list #'(v ...)))]
                 [_ #f]))
             '()))
       ;; notes/rests need a finite duration; images only need a finite start.
       (define (finite-real? x) (and (real? x) (not (infinite? x))))
       (define (note-timed? expr)
         (and (finite-real? (expr-interval-start expr))
              (finite-real? (expr-interval-end expr))))
       (define (image-timed? expr)
         (finite-real? (expr-interval-start expr)))

       ;; collect raw note/rest data per voice, plus images per voice,
       ;; clefs/keys/time-sigs/lyrics/figures/cursors per voice, and
       ;; global line-break times.  We iterate `(lookup-ctxt)` rather
       ;; than `(current-ctxt)` so that art objects placed at outer
       ;; scopes (e.g. a top-level `(clef bass)` in the enclosing
       ;; `realize` call) participate in the music as defaults — this
       ;; is the standard art lookup-ctxt protocol for crossing music
       ;; embedding boundaries.  Notes/rests/lyrics/etc. only exist
       ;; inside music blocks anyway, so the wider iteration is safe:
       ;; outer art forms (`art-section`, other `(music ...)` blocks,
       ;; titles, layouts, …) simply don't match any of the clauses
       ;; below.
       (define-values (voices+raw voices+images voices+clefs voices+keys voices+times voices+lyrics voices+figures voices+cursors voices+tuplets break-times)
         (for/fold ([events (hash)] [images (hash)] [clefs (hash)]
                    [keys (hash)] [times (hash)] [lyrics (hash)] [figures (hash)] [cursors (hash)] [tuplets (hash)] [breaks '()])
                   ([expr (lookup-ctxt)])
           (syntax-parse expr
             [({~literal note} p a o)
              #:when (note-timed? expr)
              (define start (/ (expr-interval-start expr) beat-val))
              (define end   (/ (expr-interval-end expr)   beat-val))
              (values (dict-update events (voice-name expr)
                                   (λ (lst) (cons (list start end 'note
                                                        (syntax-e #'p)
                                                        (syntax-e #'a)
                                                        (syntax-e #'o))
                                                  lst))
                                   (λ () '()))
                      images clefs keys times lyrics figures cursors tuplets breaks)]
             [({~literal music-rest})
              #:when (note-timed? expr)
              (define start (/ (expr-interval-start expr) beat-val))
              (define end   (/ (expr-interval-end expr)   beat-val))
              (values (dict-update events (voice-name expr)
                                   (λ (lst) (cons (list start end 'rest) lst))
                                   (λ () '()))
                      images clefs keys times lyrics figures cursors tuplets breaks)]
             [({~literal clef} name:id)
              ;; outer-scope clefs have no interval (they live above
              ;; the music block); treat them as defaults at t=0.
              (define raw-start (expr-interval-start expr))
              (define start (if (finite-real? raw-start) (/ raw-start beat-val) 0))
              (values events images
                      (dict-update clefs (voice-name expr)
                                   (λ (lst) (cons (list start (syntax-e #'name)) lst))
                                   (λ () '()))
                      keys times lyrics figures cursors tuplets breaks)]
             [({~literal key} p:id a:number m:id)
              (define raw-start (expr-interval-start expr))
              (define start (if (finite-real? raw-start) (/ raw-start beat-val) 0))
              (values events images clefs
                      (dict-update keys (voice-name expr)
                                   (λ (lst) (cons (list start
                                                        (syntax-e #'p)
                                                        (syntax-e #'a)
                                                        (syntax-e #'m))
                                                  lst))
                                   (λ () '()))
                      times lyrics figures cursors tuplets breaks)]
             [({~literal time-sig} n:number d:number)
              (define raw-start (expr-interval-start expr))
              (define start (if (finite-real? raw-start) (/ raw-start beat-val) 0))
              (values events images clefs keys
                      (dict-update times (voice-name expr)
                                   (λ (lst) (cons (list start
                                                        (syntax-e #'n)
                                                        (syntax-e #'d))
                                                  lst))
                                   (λ () '()))
                      lyrics figures cursors tuplets breaks)]
             [({~literal image} fn:str)
              #:when (image-timed? expr)
              (define start (/ (expr-interval-start expr) beat-val))
              (values events
                      (dict-update images (voice-name expr)
                                   (λ (lst) (cons (list start (syntax-e #'fn)) lst))
                                   (λ () '()))
                      clefs keys times lyrics figures cursors tuplets breaks)]
             [({~literal lyric} word:str)
              ;; lyrics use the `index` coordinate (set up by `ix--`/
              ;; `lyrics`) for ordering — they don't carry an interval.
              (define ix (expr-index expr))
              (define ix-key (if (null? ix) +inf.0 (car ix)))
              (values events images clefs keys times
                      (dict-update lyrics (voice-name expr)
                                   (λ (lst) (cons (list ix-key
                                                        (syntax-e #'word))
                                                  lst))
                                   (λ () '()))
                      figures cursors tuplets breaks)]
             [({~literal figure} text:str)
              ;; figures are ordered by `index` (set up by
              ;; `ix--`/`figured-bass`) and pair 1:1 with the
              ;; voice's notes in time order.
              (define ix (expr-index expr))
              (define ix-key (if (null? ix) +inf.0 (car ix)))
              (values events images clefs keys times lyrics
                      (dict-update figures (voice-name expr)
                                   (λ (lst) (cons (list ix-key
                                                        (syntax-e #'text))
                                                  lst))
                                   (λ () '()))
                      cursors tuplets breaks)]
             ;; (cursor) art-object — emit a music sign (segno) above
             ;; the staff at the cursor's start time, in the voice it
             ;; was placed in.  used to mark canon entrances / "start
             ;; here" points in short-form scores where the canon
             ;; voices aren't actually rendered.  Stored as a
             ;; `(cons start #f)` so it shares a bucket with the
             ;; arbitrary-text `(hint …)` marks below; the renderer
             ;; switches on `cdr` to pick segno vs text.
             [({~literal cursor})
              #:when (image-timed? expr)
              (define start (/ (expr-interval-start expr) beat-val))
              (values events images clefs keys times lyrics figures
                      (dict-update cursors (voice-name expr)
                                   (λ (lst) (cons (cons start #f) lst))
                                   (λ () '()))
                      tuplets breaks)]
             ;; (hint "TEXT") — emit `\mark \markup { "TEXT" }` above
             ;; the top staff at the hint's start time.  Same
             ;; segno/cursor plumbing, just text instead of a glyph.
             [({~literal hint} text:str)
              #:when (image-timed? expr)
              (define start (/ (expr-interval-start expr) beat-val))
              (values events images clefs keys times lyrics figures
                      (dict-update cursors (voice-name expr)
                                   (λ (lst) (cons (cons start (syntax-e #'text)) lst))
                                   (λ () '()))
                      tuplets breaks)]
             ;; (tuplet n d) — collect tuplet intervals per voice
             ;; for the chord-grouper to consume.  Voiceless tuplets
             ;; live under 'main and are added to whichever single
             ;; voice ends up rendering — see `tuplets-for` below.
             [({~literal tuplet} n:number d:number)
              #:when (note-timed? expr)
              (define start (/ (expr-interval-start expr) beat-val))
              (define end   (/ (expr-interval-end expr)   beat-val))
              (values events images clefs keys times lyrics figures cursors
                      (dict-update tuplets (voice-name expr)
                                   (λ (lst) (cons (list start end
                                                        (syntax-e #'n)
                                                        (syntax-e #'d))
                                                  lst))
                                   (λ () '()))
                      breaks)]
             [({~literal line-break})
              #:when (image-timed? expr)
              (define start (/ (expr-interval-start expr) beat-val))
              (values events images clefs keys times lyrics figures cursors tuplets (cons start breaks))]
             [_ (values events images clefs keys times lyrics figures cursors tuplets breaks)])))

       (define sorted-break-times (sort break-times <))

       ;; per-voice bar length (in quarter notes) used by the
       ;; bar-aware duration splitter.  if a voice has its own
       ;; (time-sig …), the earliest one wins; otherwise we fall
       ;; back to a voiceless (time-sig …) stored under 'main; if
       ;; nothing is set we treat it as un-barred and let the
       ;; splitter use a plain greedy split (the original
       ;; behavior).
       (define (voice-bar-len-of name)
         (define vt (sort (dict-ref voices+times name '()) < #:key car))
         (define gt (sort (dict-ref voices+times 'main '()) < #:key car))
         (define ts (cond [(pair? vt) (car vt)]
                          [(pair? gt) (car gt)]
                          [else #f]))
         (cond
           [ts (match-define (list _ n d) ts)
               (* n (/ 4 d))]
           [else #f]))

       ;; Find the tuplet `(ts te n d)` that fully contains the
       ;; half-open event interval [s, e); #f if none.  Tuplets that
       ;; overlap a note only partially are silently ignored — they're
       ;; only well-defined when the contained notes' boundaries
       ;; align with tuplet boundaries.
       (define (find-tuplet s e tuplets)
         (for/or ([t (in-list tuplets)])
           (match-define (list ts te _ _) t)
           (and (<= ts s) (<= e te) t)))

       ;; per-voice tuplet list: voice-specific tuplets first, then
       ;; voiceless ones (under 'main).  Same fallback rule as bar
       ;; lengths / cursors: voiceless declarations participate as
       ;; defaults across every voice.
       (define (tuplets-for name)
         (append (dict-ref voices+tuplets name '())
                 (dict-ref voices+tuplets 'main '())))

       ;; Group notes that share BOTH start and end into a single
       ;; chord event.  Result: list of (start end tok-str), sorted.
       ;; Notes with same start but different end are still emitted
       ;; separately and may overlap visually — that case is true
       ;; polyphony and would need <<\\>> voice splitting to render
       ;; correctly.
       ;;
       ;; When a chord/rest falls fully inside one of `tuplets`
       ;; (list of `(ts te n d)`), its rendered duration is scaled
       ;; by `n/d` so a 1/3-quarter triplet eighth comes out as `8`
       ;; (not `16~64~64` from the bar-aware splitter) and the
       ;; renderer emits `\tuplet n/d { … }` brackets around the
       ;; run.  Open/close brackets ride along on the FIRST / LAST
       ;; event's tok-str — the outer render loop is otherwise
       ;; tuplet-unaware.
       (define (group-into-chords raws bar-len tuplets)
         (define sorted
           (sort raws (λ (a b)
                        (or (< (car a) (car b))
                            (and (= (car a) (car b))
                                 (< (cadr a) (cadr b)))))))
         (let loop ([items sorted] [acc '()] [open-tup #f])
           (cond
             [(null? items)
              ;; if we end while still inside a tuplet, append `}`
              ;; to the last event so the bracket closes cleanly.
              (define result (reverse acc))
              (cond [(and open-tup (cons? result))
                     (match-define (list s e tok) (last result))
                     (append (drop-right result 1)
                             (list (list s e (string-append tok " }"))))]
                    [else result])]
             [else
              (define hd (car items))
              (define s (car hd))
              (define e (cadr hd))
              (define-values (same rest)
                (splitf-at items
                           (λ (it) (and (= (car it) s) (= (cadr it) e)))))
              (define dur (- e s))
              (define datas (map cddr same))
              (define cur-tup (find-tuplet s e tuplets))
              ;; Tuplet transition: close previous bracket if we're
              ;; leaving one, open a new one if we're entering one.
              ;; Cases:
              ;;   open=#f, cur=#f      → no change
              ;;   open=#f, cur=T       → open T
              ;;   open=T,  cur=T       → stay inside T
              ;;   open=T,  cur=#f      → close T (on PREVIOUS event)
              ;;   open=T1, cur=T2 (≠)  → close T1, open T2
              (define close-prev?
                (and open-tup (not (equal? cur-tup open-tup))))
              (define open-now?
                (and cur-tup (not (equal? cur-tup open-tup))))
              ;; If we need to close the previous tuplet, retroactively
              ;; append `}` to the most recent event's token.
              (define acc*
                (cond [(and close-prev? (cons? acc))
                       (match-define (list ps pe ptok) (car acc))
                       (cons (list ps pe (string-append ptok " }")) (cdr acc))]
                      [else acc]))
              (define prefix
                (cond [open-now?
                       (match-define (list _ _ n d) cur-tup)
                       (format "\\tuplet ~a/~a { " n d)]
                      [else ""]))
              (define inner-tok-str
                (cond
                  ;; any rest in the bucket (shouldn't normally mix
                  ;; with notes — but a lone rest will land here)
                  [(memf (λ (d) (eq? (car d) 'rest)) datas)
                   (cond [cur-tup
                          (match-define (list _ _ n d) cur-tup)
                          (string-join (for/list ([dd (in-list (duration->lily (* dur (/ n d))))])
                                         (string-append "r" dd)) " ")]
                         [else
                          (string-join (rest->lily-tokens dur s pickup-val bar-len) " ")])]
                  [else
                   (define pitches
                     (for/list ([d (in-list datas)])
                       (match d
                         [(list 'note p a o) (pitch->lily p a o)])))
                   (cond [cur-tup
                          (match-define (list _ _ n d) cur-tup)
                          (define body
                            (cond [(= (length pitches) 1) (car pitches)]
                                  [else (string-append "<" (string-join pitches " ") ">")]))
                          (define durs (duration->lily (* dur (/ n d))))
                          (cond [(null? durs) (string-append body "4")]
                                [(= (length durs) 1) (string-append body (car durs))]
                                [else
                                 (string-join
                                  (for/list ([dd (in-list durs)] [i (in-naturals)])
                                    (define tie (if (< i (sub1 (length durs))) "~" ""))
                                    (string-append body dd tie))
                                  " ")])]
                         [else
                          (string-join (chord->lily-tokens pitches dur s pickup-val bar-len) " ")])]))
              (define tok-str (string-append prefix inner-tok-str))
              (loop rest (cons (list s e tok-str) acc*) cur-tup)])))

       (define voices+events
         (for/list ([(name raws) (in-dict voices+raw)])
           (cons name (group-into-chords raws (voice-bar-len-of name)
                                          (tuplets-for name)))))

       ;; reorder voices: those named in ordered-voice-syms first
       ;; (in that order), then the rest in their existing order.
       (define ordered
         (let* ([by-name (for/hash ([pair (in-list voices+events)])
                           (values (car pair) (cdr pair)))]
                [requested (for/list ([v (in-list ordered-voice-syms)]
                                      #:when (hash-has-key? by-name v))
                             (cons v (hash-ref by-name v)))]
                [requested-set (list->set ordered-voice-syms)]
                [leftover (for/list ([pair (in-list voices+events)]
                                     #:unless (set-member? requested-set
                                                           (car pair)))
                            pair)])
           (append requested leftover)))

       (match-define `((,voices . ,events) ...) ordered)

       ;; per-voice: build the music body (notes / rests, plus
       ;; \break tokens for any line-breaks).  image markups are
       ;; emitted *separately* in a parallel `dance voice` of skips
       ;; (see `dance-voice-body` below) so they sit at their exact
       ;; tonart times instead of glomming onto whatever note happens
       ;; to be next.
       (define first-voice-name (and (pair? voices) (car voices)))
       (define voice-bodies
         (for/list ([name (in-list voices)] [evs (in-list events)])
           (define sorted (sort evs < #:key car))
           (define imgs (sort (dict-ref voices+images name '()) < #:key car))
           (define bar-len (voice-bar-len-of name))
           ;; cursor / hint marks for this voice — each entry is
           ;; `(cons start text-or-#f)`, with `#f` meaning the
           ;; segno glyph (placed by `cursor`) and a string meaning
           ;; arbitrary text (placed by `hint`).  cursors/hints
           ;; placed explicitly in this voice always render here;
           ;; voiceless ones (collected under the synthetic 'main
           ;; bucket) only render on the first voice — `\mark` is
           ;; a score-level event in lilypond, so emitting one per
           ;; voice would stack identical marks on top of each other.
           (define cursor-times
             (sort (append (dict-ref voices+cursors name '())
                           (if (eq? name first-voice-name)
                               (dict-ref voices+cursors 'main '())
                               '()))
                   < #:key car))

           (define-values (rendered _ _2 _3)
             (for/fold ([out '()] [cursor 0]
                        [pending-breaks sorted-break-times]
                        [pending-cursors cursor-times])
                       ([ev (in-list sorted)])
               (match-define (list s e tok-str) ev)
               (define gap (- s cursor))
               (define gap-rest
                 (if (> gap 0) (string-join (rest->lily-tokens gap cursor pickup-val bar-len) " ") ""))

               ;; cursor / hint marks fire on the first note whose
               ;; start time has caught up to the mark's time —
               ;; prepend a `\mark \markup { … }` before that note
               ;; token.  Segno glyph for `#f`-tagged entries (from
               ;; `(cursor)`), arbitrary text for string-tagged ones
               ;; (from `(hint "TEXT")`).
               (define-values (now-marking still-marking)
                 (splitf-at pending-cursors (λ (item) (<= (car item) s))))
               (define mark-tok
                 (apply string-append
                        (for/list ([item (in-list now-marking)])
                          (cond
                            [(cdr item)
                             (format "\\mark \\markup { ~s } " (cdr item))]
                            [else
                             "\\mark \\markup { \\musicglyph #\"scripts.segno\" } "]))))

               (define-values (now-breaking still-breaking)
                 (splitf-at pending-breaks (λ (t) (<= t e))))
               (define break-tok
                 (if (null? now-breaking) ""
                     (apply string-append
                            (for/list ([_ (in-list now-breaking)]) " \\break"))))
               (values (cons (string-append mark-tok tok-str break-tok)
                             (if (string=? gap-rest "") out (cons gap-rest out)))
                       e
                       still-breaking
                       still-marking)))

           (define has-images? (not (null? imgs)))
           (list name (string-join (reverse rendered) " ") has-images? sorted imgs)))

       (define staves
         (string-join
          (for/list ([row (in-list voice-bodies)])
            (match-define (list name body has-images? sorted-events imgs) row)
            (define name-str (~a name))
            (define bar-len (voice-bar-len-of name))
            ;; with the dance voice approach, all image markups live
            ;; in a parallel skip voice; we still set a generous
            ;; staff-padding so multiple stacked markups in that voice
            ;; don't collide with the staff above.
            ;;
            ;; `extra-spacing-width` is the override that actually
            ;; reaches lilypond's horizontal spacing engine — plain
            ;; `\with-dimensions` declares a markup's bbox but
            ;; SpacingSpanner ignores TextScripts by default, so
            ;; without this each dance markup is treated as 0-wide
            ;; for spacing purposes and adjacent images crash into
            ;; each other on dense rhythms.  Setting it to
            ;; `(-h-half . h-half)` forces the engine to reserve
            ;; `image-h-extent` staff-spaces of horizontal room per
            ;; dancer.  Half-extent computed in racket to avoid
            ;; lilypond rationals.
            (define h-half (/ image-h-extent 2))
            (define image-overrides
              (if has-images?
                  (sa "    \\override TextScript.outside-staff-priority = ##f\n"
                      "    \\override TextScript.staff-padding = #1\n"
                      "    \\override TextScript.padding = #0\n"
                      "    \\override TextScript.extra-spacing-width"
                      " = #'(" (exact->inexact (- h-half)) " . "
                              (exact->inexact h-half) ")\n")
                  ""))
            (define pickup-line
              (if pickup-val
                  (sa "      \\partial "
                      (string-join (duration->lily pickup-val) " ")
                      "\n")
                  ""))
            ;; Read the earliest (clef/key/time-sig …) art-object
            ;; placed in this voice and emit it at the staff opening.
            ;; Voiceless objects land under the synthetic 'main voice
            ;; and act as a default for any voice with no entry of
            ;; its own.  No object at all → no directive → lilypond
            ;; default (treble / no key / no time).
            (define (earliest dict)
              (define voice-list (sort (dict-ref dict name '()) < #:key car))
              (define global-list (sort (dict-ref dict 'main '()) < #:key car))
              (cond [(pair? voice-list) (car voice-list)]
                    [(pair? global-list) (car global-list)]
                    [else #f]))
            (define clef-line
              (cond
                [(earliest voices+clefs)
                 => (λ (e) (sa "      \\clef " (~a (cadr e)) "\n"))]
                [else ""]))
            (define key-line
              (cond
                [(earliest voices+keys)
                 => (λ (e)
                      (match-define (list _start p a m) e)
                      ;; lilypond accidentals: each sharp = "is", each flat = "es"
                      (define p-name
                        (sa (~a p)
                            (cond [(positive? a) (apply sa (build-list a (λ _ "is")))]
                                  [(negative? a) (apply sa (build-list (- a) (λ _ "es")))]
                                  [else ""])))
                      (sa "      \\key " p-name " \\" (~a m) "\n"))]
                [else ""]))
            (define time-line
              (cond
                [(earliest voices+times)
                 => (λ (e)
                      (match-define (list _start n d) e)
                      (sa "      \\time " (~a n) "/" (~a d) "\n"))]
                [else ""]))
            ;; if this voice has any (lyric …) art-objects, emit a
            ;; sibling \new Lyrics block with one quoted token per
            ;; lyric, sorted by index.
            ;;
            ;; pass `--` (syllable continuation, draws a hyphen
            ;; between two syllables of one word) and `__` (melisma
            ;; extender, draws an underscore line under multiple
            ;; notes for one syllable) through *unquoted* — those
            ;; aren't syllables themselves and don't consume a note.
            ;; everything else is quoted so apostrophes / commas /
            ;; punctuation are safe.
            (define voice-lyrics
              (sort (dict-ref voices+lyrics name '()) < #:key car))
            (define (lyric->ly w)
              (cond [(equal? w "--") "--"]
                    [(equal? w "__") "__"]
                    [else (sa "\"" w "\"")]))
            (define lyrics-block
              (cond
                [(null? voice-lyrics) ""]
                [else
                 (sa "\n  \\new Lyrics \\lyricsto \"" name-str "\" {\n"
                     "    "
                     (string-join
                      (for/list ([w (in-list (map cadr voice-lyrics))])
                        (lyric->ly w))
                      " ")
                     "\n  }")]))
            ;; figured bass: pair each (figure …) (sorted by index)
            ;; with the i'th note in this voice (sorted by time) and
            ;; emit `<text>dur` per note.  notes past the figure
            ;; list, and slots whose text is "", emit `s<dur>`
            ;; (figmode skip) — a literal empty `<>` does not
            ;; reliably advance the figure cursor in lilypond and
            ;; will visually shift later figures off their bass
            ;; notes.  multi-token durations show the figure once on
            ;; the first chunk and `s` for continuations so the
            ;; figure stays visually attached without redrawing.
            (define voice-figures
              (sort (dict-ref voices+figures name '()) < #:key car))
            (define voice-events-sorted sorted-events)
            (define figured-bass-block
              (cond
                [(null? voice-figures) ""]
                [else
                 (define fig-texts (map cadr voice-figures))
                 (define paired
                   (for/list ([ev (in-list voice-events-sorted)]
                              [i (in-naturals)])
                     (match-define (list s e _) ev)
                     (define text
                       (if (< i (length fig-texts))
                           (list-ref fig-texts i)
                           ""))
                     (define durs (duration->lily/bar (- e s) s pickup-val bar-len))
                     (define ds (if (null? durs) (list "4") durs))
                     (cond
                       [(equal? text "")
                        (string-join
                         (for/list ([d (in-list ds)]) (sa "s" d))
                         " ")]
                       [else
                        (string-join
                         (for/list ([d (in-list ds)] [j (in-naturals)])
                           (cond [(= j 0) (sa "<" text ">" d)]
                                 [else   (sa "s" d)]))
                         " ")])))
                 (sa "\n  \\new FiguredBass \\with { alignBelowContext = \"" name-str "\" } "
                     "\\figuremode {\n"
                     "    " (string-join paired " ") "\n"
                     "  }")]))
            ;; dance voice: a parallel \new Voice in the same staff
            ;; whose body is just `s<dur>` skips with image markups
            ;; attached at the exact tonart times the user specified.
            ;; this stops images from glomming onto whatever notehead
            ;; happens to be next on the staff and lets a quarter-beat
            ;; image actually land on its quarter beat.
            ;;
            ;; we walk imgs in time order, padding leading / trailing
            ;; gaps with markup-less skips and giving each image a
            ;; skip whose duration runs to the next image start (or to
            ;; the music's end for the final image).
            (define music-end
              (cond
                [(pair? sorted-events) (cadr (last sorted-events))]
                [(pair? imgs)          (+ (car (last imgs)) 1)]
                [else                  0]))
            (define (skip-tokens dur at)
              (cond
                [(<= dur 0) '()]
                [else (skip->lily-tokens dur at pickup-val bar-len)]))
            (define (attach-markup-to-first toks markup)
              (cond
                [(null? toks) (list markup)]
                [else (cons (string-append (car toks) markup) (cdr toks))]))
            (define-values (dance-pieces _last-cursor)
              (for/fold ([out '()] [cursor 0])
                        ([img (in-list imgs)] [i (in-naturals)])
                (match-define (list start fn) img)
                (define next-start
                  (cond
                    [(< (add1 i) (length imgs)) (car (list-ref imgs (add1 i)))]
                    [else                       music-end]))
                (define lead-toks (skip-tokens (- start cursor) cursor))
                (define img-toks  (skip-tokens (max 0 (- next-start start)) start))
                (define markup    (image-markup fn))
                (values (append (reverse (attach-markup-to-first img-toks markup))
                                (reverse lead-toks)
                                out)
                        next-start)))
            (define dance-voice-body
              (cond
                [(null? imgs) #f]
                [else (string-join (reverse dance-pieces) " ")]))
            (define dance-voice-block
              (cond
                [dance-voice-body
                 (sa "    \\new Voice = \"" name-str "-dance\" \\with {\n"
                     "      \\remove Note_heads_engraver\n"
                     "      \\remove Rest_engraver\n"
                     "    } {\n"
                     image-overrides
                     pickup-line
                     "      " dance-voice-body "\n"
                     "    }\n")]
                [else ""]))
            (sa "  \\new Staff = \"" name-str "\""
                " \\with { instrumentName = \"" name-str "\" } {\n"
                "    <<\n"
                "    \\new Voice = \"" name-str "\" {\n"
                clef-line
                key-line
                time-line
                pickup-line
                "      " body "\n"
                "    }\n"
                dance-voice-block
                "    >>\n"
                "  }"
                figured-bass-block
                lyrics-block))
          "\n"))

       (define any-images? (ormap caddr voice-bodies))
       (define header (sa "\\version \"2.24.0\"\n"))
       (define title-line
         (if (equal? title-val "")
             ""
             (sa "\\header { title = \"" title-val "\" }\n")))
       ;; reserve generous space between the title and the first system,
       ;; and between systems on a multi-page score, so tall image
       ;; markups don't bleed into the title area or the system above.
       (define paper-block
         (if any-images?
             (sa "\\paper {\n"
                 "  markup-system-spacing.basic-distance = #" (* image-scale 3) "\n"
                 "  markup-system-spacing.padding = #4\n"
                 "  system-system-spacing.basic-distance = #" (* image-scale 3) "\n"
                 "  system-system-spacing.padding = #4\n"
                 "  top-margin = #15\n"
                 "}\n")
             ""))
       (define score-open (sa "\\score {\n  <<\n"))
       ;; when images are present we need to push the staves apart
       ;; vertically — a tall dancer above one staff would otherwise
       ;; collide with the staff above.  staff-staff-spacing controls
       ;; the gap between consecutive staves in the same system, and
       ;; StaffGrouper.staff-staff-spacing covers the grouped case.
       (define layout-block
         (if any-images?
             (sa "  \\layout {\n"
                 "    \\context {\n"
                 "      \\Staff\n"
                 "      \\override VerticalAxisGroup.staff-staff-spacing"
                 " = #'((basic-distance . " (* image-scale 2)
                       ") (minimum-distance . " (* image-scale 3/2)
                       ") (padding . 4) (stretchability . 0))\n"
                 "      \\override VerticalAxisGroup.default-staff-staff-spacing"
                 " = #'((basic-distance . " (* image-scale 2)
                       ") (minimum-distance . " (* image-scale 3/2)
                       ") (padding . 4) (stretchability . 0))\n"
                 "    }\n"
                 "    \\context {\n"
                 "      \\Score\n"
                 "      \\override StaffGrouper.staff-staff-spacing"
                 " = #'((basic-distance . " (* image-scale 2)
                       ") (minimum-distance . " (* image-scale 3/2)
                       ") (padding . 4) (stretchability . 0))\n"
                 ;; nudge the spacing engine to treat every duration
                 ;; as if it were at least a quarter — the markup's
                 ;; `extra-spacing-width` (set per-voice above) does
                 ;; the actual horizontal reservation, so we don't
                 ;; need the more aggressive shortest-duration-space
                 ;; / uniform-stretching combo (which forced one
                 ;; measure per system).  this single override is
                 ;; enough to stop lilypond from compressing
                 ;; sixteenths so tight that the dancer markups
                 ;; can't fit.
                 "      \\override SpacingSpanner.base-shortest-duration"
                 " = #(ly:make-moment 1/4)\n"
                 "    }\n"
                 "  }\n")
             "  \\layout { }\n"))
       (define score-close (sa "\n  >>\n" layout-block "  \\midi { }\n}\n"))
       (define output
         (string-append header title-line paper-block score-open staves score-close))

       #`#,output])))
