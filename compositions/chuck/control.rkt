#lang racket

(require art tonart (for-syntax syntax/parse))

(define (init)
  (match-define (list* ck-input ck-output _) (process "chuck --shell"))
  (displayln "+ play_queued.ck" ck-output)
  (displayln "+ go.ck" ck-output)
  (displayln "+ rec.ck" ck-output)
  (flush-output ck-output)
  (values ck-input ck-output))

(define-values (ck-input ck-output) (init))

(define-for-syntax prelude 
  (box (list 
    #'(sound-map 
      [ding . "/Users/jagen31/git/tonart-compositions/compositions/chuck/ding.wav"]
      [left . "/Users/jagen31/git/tonart-compositions/compositions/chuck/low-bongo.wav"]
      [fart . "/Users/jagen31/git/tonart-compositions/compositions/chuck/fart.aifc"]
      [right . "/Users/jagen31/git/tonart-compositions/compositions/chuck/high-bongo.wav"]
      [loud . "/Users/jagen31/git/tonart-compositions/compositions/chuck/loud.aifc"]
      [mrmr . "/Users/jagen31/git/tonart-compositions/compositions/chuck/mrmr.aifc"]
      [silly . "/Users/jagen31/git/tonart-compositions/compositions/chuck/.aifc"]
      [tooloud . "/Users/jagen31/git/tonart-compositions/compositions/chuck/tooloud.aifc"])
    #'(key d 0 minor) 
    #'(voice@ (soprano) (octave 6))
    #'(voice@ (alto) (octave 5))
    #'(voice@ (tenor) (octave 4))
    #'(voice@ (bass) (octave 3))
    #'(tuning 12tet) #'(volume 5))))
(define-for-syntax postlude (box (reverse (list #'(^->note) #'(note->tone) #'(d/dt)))))
(define-for-syntax current-time (box 0))

(define-syntax (send stx)
  (syntax-parse stx
    [(_ expr ...)
     #`(begin
     (define chuck-string
       (realize (music-chuck-realizer)
         
         #,@(reverse (unbox prelude))
         (i@ [0 +inf.0] expr ...)
         #,@(reverse (unbox postlude))
         #;(translate #,(- (unbox current-time)))))
     (displayln (format "{ Go.queueMe();\n~a }" chuck-string) #,(datum->syntax stx 'ck-output))
     (flush-output #,(datum->syntax stx 'ck-output)))]))

(define-syntax (advance-time stx)
  (syntax-parse stx
    [(_ t:number)
     (set-box! current-time (+ (unbox current-time) (syntax-e #'t)))
     #'(begin
         (displayln (format "{ Go.advanceTime(~s::second); }" t) ck-output)
         (flush-output ck-output))]))

(define-syntax at (make-rename-transformer #'advance-time))
