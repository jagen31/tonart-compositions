#lang racket

(require art (for-syntax syntax/parse racket/random racket/draw))

(define-art-object (row []))

(define-art-rewriter rows
  (λ (stx)
    (syntax-parse stx
      [(_ [value ...] ...)
       (qq-art stx (ix-- (row value ...) ...))])))

(define-art resolution-list
  (rows
   [hymns "Record 30 hymns at tempo"]
   [jazz "Memorize 30 Jazz charts"]
   [arca "Make 1 PR to Arca every 2 weeks"]
   [tonart "Make 1 PR to Tonart every 2 weeks"]
   [cabin "Visit Jake's Cabin"]
   [luke-travel "Travel Somewhere outside the state with Luke"]
   [zach-travel "Travel Somewhere outside the state with Zach"]
   [demitri-travel "Travel Somewhere outside the state with Mr. Sampractice"]
   [website "Set up a personal website"]
   [host-concerts "Host 3 weird concerts"]
   [audio-setup "Orchestrate a speaker setup across 3 rooms"]
   [move-friends "Get David Luo or Bryan Brown to Boston"]
   [albums "Listen to 50 full albums"]
   [organs "Try 10 new organs"]
   [sample-organ "Create an organ sample set from a Boston organ with Josh"]
   [perform-thomas "Perform with Thomas"]
   [perform-funkwaffle "Perform with Funkwaffle"]
   [play-service "Play a piece at a service"]
   [ago-certified "Become AGO service playing ceritifed"]
   [club-passing "Learn club passing"]
   [whites "Do 5 peaks in the whites"]
   [disc-golf-score "Score even in disc golf"]
   [disc-golf-tournaments "Do 5 disc golf tournaments"]
   [volunteer "Join a volunteering organization"]
   [choir "Join a choir"]
   [tiff-fest "Play something at Tifffest 3"]
   [compose-for-nightingales "Compose for the Nightingales"]
   [musical "Write a short musical"]
   [wind-ensemble "Compose for Northeastern wind ensemble"]
   [memoir-songs "Compose 5 memoir songs for my organ friends"]
   [books "Read 10 books"]
   [dance "Record one dance a month to original compositions/improvs"]
   [casseroles "Learn to make 10 obscure casseroles"]
   [nerf-war "Have a nerf war at Hall Ave"]
   [cape-cod "Build a fort at cape cod at low tide"]
   [assimilate-work-friends "Assimilate 10 local work friends into Teevis"]
   [go-bills "Bills win the superbowl"]
   [penn "Travel to two cities in pennsylvania"]
   [study-break "Play the organ at the halloween study break"]
   [climb-purples "Climb 3 purple walls at BBP"]
   [conference-talk "Give a conference talk"]))

(define-art resolution-dependencies
  (rows
   [jazz thomas]
   [arca demitri-t]
   [cabin jake-h]
   [luke-travel luke-m]
   [move-friends david bryan]
   [sample-organ josh-c]
   [perform-thomas thomas]
   [perform-funkwaffle funkwaffle]
   [club-passing chris]
   [whites chris]
   [disc-golf-score kevin miles xavier]
   [disc-golf-tournaments kevin miles xavier]
   [tiff-fest tiffany]
   [compose-for-nightingales nightingales]
   [musical allen]
   [wind-ensemble allen]
   [memoir-songs christian]
   [nerf-war jake-k]
   [cape-cod tim]))
  

(define-art-embedding (bingo [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (context expr ...)))])))

(define-art-rewriter pick-rows
  (λ (stx)
    (syntax-parse stx
      [(_ n:number)
       (define my-rows (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'row))
       (define selected-rows (random-sample my-rows (syntax-e #'n) #:replacement? #f))
       #`(context #,@(map delete-expr my-rows) #,@selected-rows)])))

(define-art-rewriter symbols
  (λ (stx)
    (syntax-parse stx
      [(_ sym ...)
       (qq-art stx (ix-- (symbol sym) ...))])))

(define-art card
  (ix--
    (symbols tiff-fest jazz cabin sample-organ go-bills)
    (symbols casseroles play-service nerf-war hymns organs)
    (symbols audio-setup wind-ensemble memoir-songs cape-cod compose-for-nightingales)
    (symbols website disc-golf-score climb-purples club-passing tonart)
    (symbols arca penn study-break albums conference-talk)))

(define-art resolution-short-names
  (rows
   [hymns "Record Hymns"]
   [jazz "Memorize Jazz Standards"]
   [cabin "Visit Jake's Cabin"]
   [sample-organ "Boston Virtual Organ"]
   [go-bills "Bills win the Superbowl"]
   [tiff-fest "Perform at TiffFest"]
   [play-service "Play a piece at a service"]
   [nerf-war "Host a NERF war"]
   [casseroles "Bake Obscure Casseroles"]
   [organs "Try New Organs"]
   [audio-setup "Rig the house"]
   [wind-ensemble "Write for NU Wind Ensemble"]
   [memoir-songs "Write Memoir Songs"]
   [cape-cod "Defend a fort at high tide"]
   [compose-for-nightingales "Write for Nightingales"]
   [website "Make my website"]
   [disc-golf-score "Even in disc golf"]
   [climb-purples "3 purple boulders"]
   [club-passing "Club Passing"]
   [tonart "Commit to Tonart"]
   [arca "Commit to Arca"]
   [study-break "Play at Halloween Study Break"]
   [albums "Listen to full albums"]
   [conference-talk "Give a conference talk"]))

(define-art-rewriter bitmaps
  (λ (stx)
    (syntax-parse stx
      [(_ sym ...)
       #:with (bm ...) (for/list ([sym (syntax->list #'(sym ...))]) #`(bitmap #,(save-png (read-bitmap (syntax-e sym)))))
       (qq-art stx (ix-- bm ...))])))

(define-art card2
  (ix--
   (bitmaps "lilypad.png" "jazz.jpg" "cabin.jpg" "sampling.jpg" "bills.jpg")
   (bitmaps "casserole.jpg" "church.jpeg" "nerf.jpg" "hymns.jpeg" "ohs.jpeg")
   (bitmaps "setup.jpg" "windband.jpg" "song.png" "sandfort.jpeg" "nightingales.png")
   (bitmaps "site.png" "dg.jpeg" "boulder.jpeg" "passing.jpeg" "button.png")
   (bitmaps "tonality.jpg" "penn.png" "harvard.jpg" "album.jpeg" "farm.jpg")))

(define-art xs
  (ix--
   (bitmaps "x.png" "x.png" "x.png" "blank.png" "blank.png" )
   (bitmaps "blank.png" "x.png" "blank.png" "blank.png" "x.png" )
   (bitmaps "blank.png" "blank.png" "blank.png" "blank.png" "blank.png" )
   (bitmaps "x.png" "blank.png" "blank.png" "blank.png" "x.png" )
   (bitmaps "blank.png" "blank.png" "blank.png" "blank.png" "x.png" )))