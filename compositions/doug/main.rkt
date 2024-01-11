#lang scribble/lp2

@(require scribble/manual)
 
@title[#:style manual-doc-style]{Digital Doug}

OK, here is a jam that I came up with today.  It was designed as both a mood lifter and an etude so I could
develop better hand independence, especially with syncopation in the left hand.  In this file I'm going
to lay out the main elements of it, show some 'subjams' I needed to learn to work up to it, and show
some variations.

Here are the main elements:

@chunk[<the-music>
  (interpretation+ main

    [the-comp-rhythm (rhythm 1.5 1 1 0.5 1 0.5 2.5)]
    [the-comp-harmony 
      (relative-harmony (major) [M 7] (major) [m 7] (major))]

    [the-solo-rhythm (i@ [0 8] (uniform-rhythm 0.5))])]

The comp has a syncopated rhythm and a harmony, and the solo is just constant eighth notes.

(the harmony is specified in 'harmonic relativity', if you've never seen it.  It indicates- starting 
on a major chord, jump a major 7th and play another major chord.  Then jump a minor 7th and play another 
major chord.  Note- in practice you'll actually be jumping the inverse- so a minor 2nd and a major 2nd.
Just for clarity, the exact chords I was mainly practicing with were E major, Eb major, Db major)

Here is the jam:

@chunk[<the-music>
  (interpretation+ main
    [the-composition
      (i@ (0 8)
        (key+starting-chord-related-by-aug-5th right-hand left-hand)
        (voice@ (left-hand) (the-comp-harmony) (the-comp-rhythm))
        (voice@ (right-hand) (the-solo-rhythm))
        (interpret main))])]

`key+starting-chord-related-by-aug-5th` is obviously not a standard library rewriter, it is something 
specific to our composition.  It indicates that the key of the solo is related to the starting chord 
of the comp by an augmented 5th.  The jam is polytonal, but the home key of the comp is really just a 
perfect 4th away from the solo key, which is less crazy sounding.  Concretely, if solo key = Ab
then comp starting chord = E (A5) and comp key/final chord = Db (P4).


@chunk[<the-definitions>
  ;; the object
  (define-art-object (key+starting-chord-related-by-aug-5th [key-voice chord-voice]))
  ;; the interpretation of the object
  (define-mapping-rewriter (->key+starting-chord [(: expr key+starting-chord-related-by-aug-5th)])
    (λ (stx expr)
      ;; the rewriter provides the starting key, the object provides the voices
      (syntax-parse (list stx expr)
        [((_ key-pitch:id key-accidental:number) (_ key-voice chord-voice))
         (define p+a 
           (transpose-by-interval (syntax-e #'key-pitch) (syntax-e #'key-accidental) 5 'augmented))
         (define-values (p* a*) (values (car p+a) (cadr p+a)))

         (with-syntax ([p p*] [a a*])
           (qq-art expr
             (|@| () 
               (voice@ (chord-voice) (pitch p a))
               (voice@ (key-voice) (key key-pitch key-accidental major)))))])))
]

This is the official composition, everything before is considered scaffolding or composite parts.
Everything after is moving us towards a specific realization of the composition, either as a score,
or as a computer performance.  Oh, or a practice exercise.

Here is the version to perform off of, which rewrites the composition slightly and specifies it to be 
in Ab/Db.

@chunk[<the-music>
  (interpretation+ main
    [the-composition-for-perf
      (i@ (0 8)
        (the-composition)
        (interpret main)
        ;; it's in a flat! (and d flat)
        (->key+starting-chord a -1)
        (voice@ (left-hand) 
          (relative-harmony->chord-seq)
          ;; this applies the given rhythm directly to a seq in the score, as opposed to the usual 
          ;; `apply-rhythm`, which applies a rhythm in the score to a seq in the score.
          (apply-rhythm* 4 1.5 2.5)))])]

Here is a further refinement which can be compiled directly into code to run on the sampler.  It loops
the composition 8 times (16 measures) and plays a arbitrary 4 measure melody, transposing it down 1 
diatonic step each time.  Note that none of this is part of the composition, it's just one way to
realize it [[ and it's not a very good way! :) ]].

@chunk[<the-music>
  (interpretation+ main
    [the-composition-for-computer-perf
      (i@ (0 64)

        ;; loop it every 8 beats (for a total of 8 times.  Sorry for the confusion, the 8 means every 8 beats.)
        (loop 8 (the-composition-for-perf))
        (expand-loop)
        (interpret main)

        (voice@ (right-hand) 
          ;; loop this every 16
          (loop 16
            ;; just some sequence
            (-- [8 (seq (ix-- (^ 1) (^ 2) (^ 3) (^ 2) (^ 1) (^ 0) (^ 1) (^ 2) (^ 3) (^ 4) (^ 5) (^ 6) (^ 7) (^ 8) (^ 5) (^ 8)))]
                [8 (seq (ix-- (^ 1) (^ 4) (^ 6) (^ 4) (^ 6) (^ 8) (^ 6) (^ 8) (^ 11) (^ 8) (^ 6) (^ 8) (^ 6) (^ 4) (^ 1) (^ 4)))]))
          (expand-loop)
          (apply-rhythm)
          (-- [16] [16 (transpose-diatonic 1)] [16 (transpose-diatonic -1)] [16])
          (run-transpose-diatonic)
          (apply-rhythm) (octave 5) (^->note))
        
        (voice@ (left-hand) 
          ;; map the chords to the comp rhythm.
          (rhythm->holes)
          (fill-holes chord)
          ;; write out the chords as notes
          (chord->notes/simple 3))


        ;; midi things
        (note->midi)
        (instrument-map [organ . 000/000_Montre_8]
                        [trumpet . 000/065_Quintadena_8])
        (voice@ (left-hand) (instrument organ))
        (voice@ (right-hand) (instrument trumpet))

        ;; convert measure intervals to raw beat intervals
        (metric-interval->interval)
        (midi->full-midi)
        (tempo 120)
        (apply-tempo)
        ;; convert to on/off events
        (d/dt))
        
      ])]

@chunk[<*>
  (require (except-in art number) (except-in tonart number attribute) tonart/linuxsampler
    racket
    (for-syntax racket syntax/parse racket/match tonart/liszt))

  <the-definitions>
  <the-music>
  #;(displayln (realize quote-realizer (put (the-composition-for-computer-perf)) (interpret main)))

  (displayln 
    (realize (quote-realizer)
      (the-composition-for-perf)
      (interpret main)
      (voice@ (left-hand) (rhythm->holes) (fill-holes chord))))

  (define result 
    (realize (linuxsampler-realizer) 
      (the-composition-for-computer-perf)
      (interpret main)))
       
  (define file (open-output-file "doug.cpp" 
                                   #:exists 'replace))
  (displayln result file)
  (close-output-port file)]
