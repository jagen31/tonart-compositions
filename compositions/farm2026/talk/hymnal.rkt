#lang racket

;; A little hymnal.
;;
;; Lives in its own module rather than inside preview-demo.rkt: that
;; file is an open DrRacket buffer, and a stale buffer save has already
;; wiped this once.  A library is the right home for it anyway -- the
;; hymnal is not scratch work.
;;
;; Use it by requiring this file and putting `hymnal` in scope
;; alongside the hymn numbers:
;;
;;   (define-art order-of-service
;;     hymnal
;;     (hymn-number 3) (hymn-number 8)
;;     (hymn-number->title))

(require (except-in tonart direction transpose-octave)
         (except-in "../../scribble/scribble.rkt" insert)
         ;; `select` collides with tonart's and `row` with the seq
         ;; combinator, so take only the table vocabulary used here.
         (only-in datenart table column-names column-art-types
                  column-sql-types row)
         ;; The MusicXML-loaded tune for hymn 9, already notes.
         (only-in "pldi.rkt" [theme pldi-theme])
         (for-syntax syntax/parse racket/list racket/string
                     (only-in art/private/core get-id-ctxt context-ref)))

(provide hymnal hymnal-header hymnal-rows
         hymn-number hymn-number->title hymn-number->score
         rebuild program program-title)

;; Overrides the directory-name default the preview uses for the
;; document title when this file is previewed on its own.
(define program-title "PL Songbook")
;; ===================================================================
;; A little hymnal
;; ===================================================================
;;
;; tonart's own `hymnal-table` (private/church/hymn.rkt) indexes tunes
;; by name and carries only rhythm / ^s / meter.  That is the tune half
;; of a hymnal and not the hymnal half: what a congregation actually
;; looks up is a *number*, and what a hymnal prints next to it is the
;; first line, the tune name in small caps, the meter, and the
;; attributions.  Those are different things -- one tune serves many
;; texts (ST. ANNE and NETTLETON each carry dozens) -- so number and
;; text live here, and the tune column joins across to the music.
;;
;; Meter is the join key in real hymnbooks: any 8.7.8.7 D text can be
;; sung to any 8.7.8.7 D tune, which is what the metrical index at the
;; back of a hymnal is for.  Keeping `meter` on the row makes that
;; index a `select` away.

;; The tune is stored as `melody`: notes, ready to engrave.  Splitting
;; it into rhythm and scale degrees only to glue them back with
;; `(apply-rhythm) (^->note)` in the score rewriter bought nothing, and
;; a tune loaded whole from MusicXML (`musicxml->tonart`) has no natural
;; rhythm/degrees split to give.  One column, one representation.
(define-art hymnal-header
  (@ [(table hymnal)]
     (column-names number title tune meter author composer season
                   melody lyrics)

     (column-art-types number string symbol meter string string symbol
                       (music note) (music lyrics))
     (column-sql-types (integer) (varchar 60) (varchar 24) (varchar 16)
                       (varchar 48) (varchar 48) (varchar 16)
                       (varchar) (varchar))))

;; Reconstitute a tune stored as scale degrees + rhythm into notes:
;; the four steps the score rewriter used to apply inline, now spliced
;; into each melody cell right after its `-^s` and `-rhythm`.  A tune
;; that is already notes (hymn 9) skips this.
;;
;; It has to be a *rewriter*: `define-art` evaluates `apply-rhythm` and
;; `^->note` immediately, against `rebuild`'s own empty context, so as a
;; plain art they would fire with no notes present.  As a rewriter they
;; expand in place and run against whatever surrounds them.
(define-art-rewriter rebuild
  (lambda (stx)
    #'(context (key d 0 major) (octave 5) (apply-rhythm) (^->note))))

;; The hymns.  A tune stored as scale degrees + rhythm ends its melody
;; cell with `(rebuild)`; a tune loaded whole from MusicXML (hymn 2)
;; drops straight in as notes.
(define-art hymnal-rows
  (@ [(table hymnal)]
     (row (number 3)  (string "Come to FARM and hear our music")
          (symbol nettleton) (meter 8 7 8 7 D)
          (string "Jared Gentner, 2026")
          (string "Wyeth's Repository of Sacred Music, 1813")
          (symbol general)
          (music (seq
                  (ix-- (music (i@ [0 6] (music-rest)))
                        (music nettleton-rhythm nettleton-^s (rebuild))))
                 (inline-music-seq)
                 (dilate 1/2))
          (music (lyrics "Come" "to" "FARM" "and" "hear" "our" "mu" "sic"
                         "we" "have" "art" "for" "you" "to" "see"
                         "All" "things" "ti" "dal," "rocq" "re" "ci" "tal,"
                         "One" "bit" "songs" "and" "he" "rald" "ry."

                         "on" "the" "" "eve" "ning" "of" "per" "for" "man" "ces,"
                         "see" "our" "" "mind" "ex" "pan" "ding" "show!"

                         "Learn" "what's" "to" "nal," "try" "some" "dan" "ces"
                         "on" "our" "farm," "your" "art" "will" "grow.")))

     (row (number 2)  (string "This is still PLDI")
          (symbol |always with you|) (meter 7 7 7 7 D)
          (string "John Wickerson, 2020")
          (string "Wayne Carson, 1971")
          (symbol general)
          (music pldi-theme (dilate 1/2))
          (music (lyrics "May" "be" "I" "won't" "be" "fly" "ing"
                         "On" "an" "air" "bus" "three" "" "thir" "ty"
                         "May" "be" "I" "won't" "be" "travel" "ling"
                         "on" "a" "tube" "that's" "packed" "and" "dir" "ty."
                         "but" "I'm" "here" "on" "my" "lap" "top"
                         "and" "I" "found" "me" "some" "WI" "FI" "" ""
                         "this" "is" "still" "P" "L" "D" "I"
                         "this" "is" "still" "P" "L" "D" "I")))))

(define-art hymnal hymnal-header hymnal-rows)

;; -------------------------------------------------------------------
;; hymn-number, and the two ways it compiles
;; -------------------------------------------------------------------

;; What you write in a program.  `(hymn-number 2)` and nothing else --
;; the hymnal knows the rest.
(define-art-object (hymn-number [n]))

(begin-for-syntax

  ;; Pull the hymnal's rows out of the surrounding context.  Rows carry
  ;; the `(table hymnal)` coordinate, so a `hymn-number` sitting
  ;; anywhere in the program can still find them as long as the hymnal
  ;; is in scope.
  (define (hymnal-rows-in ctxt)
    (filter (λ (e) (syntax-parse e
                     [({~literal row} _ ...)
                      (syntax-parse (context-ref (get-id-ctxt e) #'table)
                        [({~datum table} {~datum hymnal}) #t]
                        [_ #f])]
                     [_ #f]))
            ctxt))

  ;; Column order is the one declared in `hymnal-header`.  Kept as a
  ;; list here rather than read back off `column-names` because the
  ;; lookup has to work from wherever the `hymn-number` sits, which is
  ;; outside the table's own coordinate.
  (define hymnal-columns
    '(number title tune meter author composer season melody lyrics))

  (define (row-cell r col)
    (define ix (index-of hymnal-columns col))
    (and ix (list-ref (cdr (syntax->list r)) ix)))

  (define (cell-value c)
    (syntax-parse c
      [({~literal number} n) (syntax-e #'n)]
      [({~literal string} s) (syntax-e #'s)]
      [({~literal symbol} s) (syntax-e #'s)]
      [({~literal meter} p ...) (syntax->datum #'(p ...))]
      [_ #f]))

  ;; Find the row for a number, or fail with a message that names the
  ;; hymns that *do* exist -- a wrong hymn number is the likeliest
  ;; mistake here and "no such row" would not help anyone.
  (define (lookup-hymn stx n)
    (define rows (hymnal-rows-in (lookup-ctxt)))
    (or (findf (λ (r) (equal? n (cell-value (row-cell r 'number)))) rows)
        (raise-syntax-error
         'hymn-number
         (format "no hymn ~a in the hymnal (have: ~a)"
                 n (string-join
                    (for/list ([r (in-list rows)])
                      (format "~a" (cell-value (row-cell r 'number))))
                    ", "))
         stx)))

  ;; Hymnals print the tune in small caps followed by the meter, the
  ;; syllable counts joined by dots -- NETTLETON 8.7.8.7 D.  `D`
  ;; (doubled) is a modifier on the meter, not a count, so it hangs off
  ;; the end after a space rather than joining the dotted run.
  (define (meter-label parts)
    (define-values (counts mods) (partition number? parts))
    (string-append
     (string-join (map (λ (p) (format "~a" p)) counts) ".")
     (if (null? mods)
         ""
         (string-append " " (string-join (map (λ (m) (format "~a" m)) mods) " ")))))

  (define (tune-label r)
    (format "~a ~a"
            (string-upcase (symbol->string (cell-value (row-cell r 'tune))))
            (meter-label (cell-value (row-cell r 'meter))))))

;; Display form: the line a program or an order of service shows.
(define-mapping-rewriter (hymn-number->title [(: h hymn-number)])
  (λ (stx h)
    (syntax-parse h
      [(_ n:number)
       (define r (lookup-hymn h (syntax-e #'n)))
       (qq-art h (text #,(format "SONG ~a. ~a"
                                 (cell-value (row-cell r 'number))
                                 (cell-value (row-cell r 'title)))))])))

;; Full form: the heading a hymnal page carries, with the tune and
;; meter, and the music engraved underneath it.
(define-mapping-rewriter (hymn-number->score [(: h hymn-number)])
  (λ (stx h)
    (syntax-parse h
      [(_ n:number)
       (define r (lookup-hymn h (syntax-e #'n)))
       (define melody-cell (row-cell r 'melody))
       (define lyrics-cell (row-cell r 'lyrics))
       ;; The melody cell already holds notes -- either a MusicXML tune
       ;; or a degrees/rhythm pair followed by `rebuild` -- so it is
       ;; spliced straight in with the lyrics beside it.
       (syntax-parse #`(#,melody-cell #,lyrics-cell)
         [(({~literal music} mel ...) ({~literal music} lyr ...))
          (qq-art h
            (ix--
             (text #,(format "~a. ~a" (cell-value (row-cell r 'number))
                             (cell-value (row-cell r 'title))))
             (text #,(tune-label r))
             (text #,(format "~a / ~a"
                             (cell-value (row-cell r 'author))
                             (cell-value (row-cell r 'composer))))
             (music mel ... lyr ...)))])])))

;; -------------------------------------------------------------------
;; a standalone preview of the whole book
;; -------------------------------------------------------------------

;; Expands to a `hymn-number` for every row in the hymnal, in number
;; order -- so `program` below doesn't have to be edited when a hymn is
;; added.  Reuses the same row lookup the score rewriter does.
(define-art-rewriter all-hymns
  (λ (stx)
    (define rows (hymnal-rows-in (lookup-ctxt)))
    (define nums (sort (map (λ (r) (cell-value (row-cell r 'number))) rows) <))
    (with-syntax ([(hn ...) (map (λ (n) #`(hymn-number #,n)) nums)])
      (qq-art stx (ix-- hn ...)))))

;; `program` engraves every hymn in the book.  It is deliberately NOT
;; provided: requiring the hymnal into an order of service should bring
;; the table and the `hymn-number` machinery, not a `program` that
;; would collide with the caller's own.  It exists so this file is
;; something you can preview on its own -- the render chain matches what
;; a concert program does to a hymn (`inline-music-seq`, then the
;; per-voice octaves and the final `^->note`).
(define-art program
  hymnal
  (@ [(art-section Hymnal)]
     (ix-- (hymn-number 3) (hymn-number 2)))
  (hymn-number->score)
  (key d 0 major)
  (inline-music-seq)
  (dilate 1/2)
  (voice@ [soprano] (octave 5))
  (voice@ [bass] (octave 4) (clef bass))
  (^->note))
