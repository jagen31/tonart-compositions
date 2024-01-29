#lang racket
#|

DSLs and you (yes, you!)

1. What is (and is NOT) a DSL?
2. Why do we embed DSLs?  Deep vs. shallow? Initial vs. final?
3. What does racket do that other languages don't?
4. How do you program with syntax?
5. How do you program with syntax in the "Real World"?

1.
Common features:
the `Exp` (expression, expr, statement) - A type containing all possible expressions which composes into itself
the `Val` (value) - A type containing all possible expressions which cannot be reduced any further
the `Env` (context, environment) - A type containing a mapping of "name" to value.  or type.  or expression.  or something else.
the `State` (store, reference cells) - A type containing mutable 
the `eval` (evaluator, interpreter) - A function which takes an expression, environment(s), store(s) and returns a value

eval(expr: Exp, env: Env, sto: State) -> (Val, State)

the `compile` (compiler) - A function which takes an expression, environment(s), store(s) and returns syntax of another language

compile(expr: Exp, env: Env, sto: State) -> String (???)

Weirder, trickier things...

the `Module` (mod) - A unit of code which requires from modules and provides bindings to other modules

Modules provide a place for interfaces, types, contracts
They also typically have an interpretation around linking, loading, and compilation

Example: Separate compilation- Compiler concept.  Can compile some unit of code (called "translation unit" in C/C++ parlance)
without any of its dependencies present.

Racket's separate compilation - compile Modules separately
GHC separate compilation - Modules + interface files (types can't be separately compiled, but code can)

Classic example: 
MLTon is a whole program optimizing SML compiler (no separate compilation)
SML/NJ is built around interactive features, such as REPL, cached compilation

people would develop in SML/NJ and then compile with MLTon to get a fast binary.

more broadly understood example- 
static linking (everything gets thrown in the same binary, including dependencies)
vs
dynamic linking (your code goes in the binary, but dependencies are loaded at link time)

the `Binding` (reference, identifier, variable [please dont use this one]) - A name used to reference _something_ (often can be "looked up" in an `Env`)
the `Scope` (lexical scope) - static relationship between definitions and uses in a program
the `Extent` (dynamic extent) - dynamic relationship between definitions and uses in a program

the `Type System` (type checker) - A classification system for expressions, which can be computed "statically"
TRADITIONAL: describes properties of the values it may evaluate to. Rejects incoherent programs.
MODERN: Does what a traditional type checker does.  But also describes properties of the computation
(such as side effects), and also helps construct the program 
(typecheckers delivering values of a type (scala implicits), doing ad-hoc polymorphism (haskell typeclasses)).


When you find yourself talking about these things a lot, you are probably working on a DSL.


2.

Two classic embeddings:

Initial embedding- do it with an AST
Final embedding- do it with the host language

Having to design all those features is horrible and tedious.  And without these systems, languages
"do not scale".  Great minds have spent years designing, implementing, and perfecting these systems.

Examples-
Inherit lexical scope: HOAS.  PROBLEM- can't see inside the lambdas (no introspection)
Inherit typechecking: tagged representations.  Parametric HOAS.  Tagless final


|#

(module old-music-form music-lang/lib/music
  (chord-names
   I [0 4 7]
   I [0 4]
   V7/IV [0 4 7 10]
   ii [2 3 7]
   ii6 [5 4 9]
   V [7 4 7]
   V7 [7 4 7 10]
   V/V [2 4 7]
   iii [4 3 7]
   I6 [4 3 8]
   I6 [4 8]
   viio [11 3 6]
   viio6 [2 3 9]
   vi [9 3 7]
   IV [5 4 7])
  
  (progressions
   (I (ii I iii ii6 viio6 vi V/V))
   (vi V/V)
   (V (I V7/IV V I))
   (ii V)
   (I6 (I6 ii6))
   (V/V V)
   (iii V)
   (V7/IV IV)
   (viio6 I6)
   (ii6 V)
   (V7 I))
  
  (pivots
   (V/V V)
   (V7/IV V7))
  
  (phrases
   (2 C (cadence G ((V I))))
   (2 G (cadence C ((V7 I) (V I))))))





(module old-music-score (submod ".." old-music-form)
  (voice C 4 4 (E5 D5    C5                D5) (D5 E5 F#5 G5) (G5 F#5 F#5 F5)  (rest D5  D5 C5))
  (voice C 4 4 (G4 F4    (1/8 E4) (1/8 C4) A4) (G4 G4 A4  B4) (B4 B4  A4  B4)  (C5   A4  B4 G4))
  (voice C 4 4 (C4 B3    C4                A3) (B3 C4 D4  D4) (D4 D4  D4  D4)  (E4   D4  D4 E4))
  (voice C 4 4 (C3 D3    E3                F3) (G3 C3 D3  G3) (G3 B2  D3  G3)  (C3   F3  G3 C3))
;; C major    I  viio6 I6                ii6  V  I  V/V                 V7    I    ii6 V  I
;; G major                                          V   I    I  iii V   V7/IV
)

(require (for-syntax (except-in racket rest) 'old-music-score))
(require (for-syntax music-lang/lib/repr))
(require (except-in tonart ref))

(define-art-rewriter summon-score
  (λ (_)
    (define voices
      (for/fold ([acc '()])
                ([v score] [i (in-naturals)])
        (match v
          [(voice _ _ (list (measure (list (and items (or (note _ _ _ _) (rest _ _))) ...) _) ...))
           (define notes
             (for/list ([item (flatten items)])
               (match item
                 [(note (pitch-class pitch accidental) octave duration beat)
                  #`(i@ [#,beat #,(+ beat (* duration 4))]
                      (note #,(string->symbol (string-downcase (symbol->string pitch))) #,(match accidental ['sharp 1] ['flat -1] ['none 0]) #,octave))]
                 [(rest _ _) #'(context)])))
           (cons #`(voice@ [#,(string->symbol (format "voice~a" i))] #,@notes) acc)])))
    #`(context #,@voices)))

(module eval1 typed/racket

    (struct num-lit [(n : Number)] #:transparent)

    (define-type Exp num-lit)

    ;; val
    (struct numV [(n : Number)] #:transparent)

    (define-type Val (U numV))

    (define (eval [e : Exp]) : Val
      (match e
        [(num-lit n) (numV n)]))
    
    (provide (all-defined-out)))

(require (prefix-in e1: 'eval1))


(module eval2 typed/racket


    (struct num-lit [(n : Number)] #:transparent)
    (struct bool-lit [(b : Boolean)] #:transparent)
    (struct add [(l : Exp) (r : Exp)] #:transparent)
    (struct if [(c : Exp) (t : Exp) (e : Exp)] #:transparent)

    (require (prefix-in rack: (only-in racket if)))


    (define-type Exp (U num-lit bool-lit if add))

    ;; val
    (struct numV [(n : Number)] #:transparent)
    (struct boolV [(b : Boolean)] #:transparent)

    (define-type Val (U numV boolV))

    (define (eval [e : Exp]) : Val
      (match e
        [(num-lit n) (numV n)]
        [(bool-lit n) (boolV n)]
        [(add l r) (numV (+ (numV-n (eval l)) (numV-n (eval r))))]
        [(if c t e) (rack:if (boolV-b (eval c)) (eval t) (eval e))]))
    
    (provide (all-defined-out)))

(require (prefix-in e2: 'eval2))

(module blah typed/racket
  
    (struct lit [(n : Number)])
    (struct fun [(arg : Symbol) (body : Exp)])
    (struct app [(head : Exp) (arg : Exp)])
    (struct ref [(name : Symbol)])

    (define-type Exp (U lit fun app ref))

    (struct numV [(n : Number)] #:transparent)
    (struct cloV [(arg : Symbol) (body : Exp) (env : Env)] #:transparent)

    (define-type Value (U numV cloV))
    (define-type Env (Listof (Pairof Symbol Value)))

    (: eval (Exp Env -> Value))
    (define (eval e env)
      (match e
        [(lit n) (numV n)]
        [(ref n) (match (assoc n env)
                   [(cons x v) v]
                   [else (error 'eval "unbound variable")])]
        [(fun arg body) (cloV arg body env)]
        [(app head arg)
         (match (eval head env)
           [(cloV arg* body env)
            (eval body (cons (cons arg* (eval arg env)) env))]
           [v (error 'eval "expected a function, got ~a" v)])]))
           
    (provide (all-defined-out)))

(require 'blah)