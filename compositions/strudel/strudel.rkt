#lang at-exp racket

(require tonart (for-syntax syntax/parse racket/string racket/format racket/match racket/dict))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-for-syntax (sa . str)
  (apply string-append (map ~a str)))

(define-art-realizer music-strudel-realizer
  (λ (stx)
    (define len (for/fold ([n 0])
                          ([expr (current-ctxt)])
                   (syntax-parse expr
                     [({~literal loop} n* expr ...)
                      (max (syntax-e #'n*) n)])))

    (match-define `((,names . ,exprs) ...)
      (for/fold ([acc (hash)] #:result (hash->list acc))
                ([outer-expr (current-ctxt)])
        (syntax-parse outer-expr
          [({~literal loop} n expr ...)
           (define inner-len (syntax-e #'n))
           (define exprs
               (sort (run-art-exprs (syntax->list #'(expr ...)) '())
                       (λ (a b)
                         (< (expr-interval-start a)
                            (expr-interval-start b)))))
           (define notes (for/fold ([acc '()] #:result (reverse acc))
                                   ([expr exprs])
                           (syntax-parse expr
                             [({~literal note} p a o)
                              (define/syntax-parse {~or (_ name) name} (get-context exprs expr #'instrument))
                              (define/syntax-parse {~or (_ vol) vol} (get-context exprs expr #'volume))
                              (define start (expr-interval-start expr))
                              (define end (expr-interval-end expr))

                              (define inst-str (if (syntax-e #'name) (format ".s(~s)" (syntax-e #'name)) ".s(\"square\")"))
                              (define vol-str (if (syntax-e #'vol) (format ".gain(~s)" (syntax-e #'vol)) ""))

                              (define a* (match (syntax-e #'a) [0 ""] [1 "#"] [-1 "b"] [2 "x"] [-2 "bb"] [3 "###"] [4 "####"] [-3 "bbb"] [-4 "bbbb"]))
                            (cons @sa{timecat([@start, silence], [@(- end start), note("@(syntax-e #'p)@|a*|@(syntax-e #'o)")@|inst-str|@|vol-str|], [@(- inner-len end), silence]).fast(@len / @inner-len)} 
                                    acc)
                                #;(cons @sa{note("@(syntax-e #'p)@|a*|@(syntax-e #'o)")@|inst-str|@|vol-str|.outside(@len, x=>x.late(@start).clip(@(- end start)))} acc)]
                             [_ acc])))

           
                             
           (dict-update acc (expr-name outer-expr) (λ (x) (cons @sa{@(string-join notes ",\n")} x))
                        (λ () '()))]
          [_ acc])))
    (define header
      @sa|{
        samples({
'sax': 'sax/000_notes121a.wav',
'moog': { 'g3': 'moog/005_Mighty%20Moog%20G3.wav' },
}, 'github:tidalcycles/dirt-samples');

register('humanize', (amt, pat) => {
  const amtC = clamp(amt, 0, 1);
  return pat.withHaps((haps) => {
    return haps.map((hap) => {
      const offset = 0.1 * amtC * (2 * Math.random() - 1);
      return hap.withSpan((span) => span.withTime(t => t + offset));
    })
  }).withValue((v) => ({ ...v, velocity: (v.velocity ?? 1) + 0.5 * amtC * (2 * Math.random() - 1) }));
});
      }|)
    #`#,(string-append header "\n" 
          (string-join (for/list ([name names] [expr exprs]) 
                       @sa{let @(if (null? name) "_main" (syntax-e (car name))) = stack(@(string-join expr ",\n")).slow(@len)})))))
         
(define-art-object (dynamic [name]))
(define-art-object (crescendo [left-dynamic right-dynamic]))

(define-for-syntax (dynamic->volume dyn)
  (match dyn
    ['ppp 0.3]
    ['pp 0.5]
    ['p 0.7]
    ['mp 0.9]
    ['mf 1.1]
    ['f 1.3]
    ['ff 1.5]
    ['fff 1.7]
    [_ 1]))

(define-art-rewriter do-dynamics
  (λ (stx)
    (define dynamics (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'dynamic))
    (define crescendos (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'crescendo))

    (define dyn-volumes
      (for/list ([dyn dynamics])
        (qq-art dyn
          (volume #,(syntax-parse dyn
          [({~literal dynamic} name)
           (define vol (dynamic->volume (syntax-e #'name)))
           vol])))))

    (define crescendo-volumes
      (for/fold ([vols '()] #:result (reverse vols))
                ([cres crescendos])
        (syntax-parse cres
          [({~literal crescendo} left-dynamic right-dynamic)
            (define left-vol (dynamic->volume (syntax-e #'left-dynamic)))
            (define right-vol (dynamic->volume (syntax-e #'right-dynamic)))
            (define steps 10)
            (define step-size (/ (- right-vol left-vol) steps))
            (define start (expr-interval-start cres))
            (define end (expr-interval-end cres))
            (define/syntax-parse (vol-clause ...)
              (for/fold ([acc '()] #:result (reverse acc))
                        ([i (in-range steps)])
                (define vol (+ left-vol (* i step-size)))
                (cons #`[#,(* i (/ (- end start) steps)) (volume #,vol)] acc)))
            (cons (qq-art cres (-- vol-clause ...)) vols)])))

    #`(context #,@(map delete-expr dyn-volumes) 
               #,@(map delete-expr crescendo-volumes) 
               #,@dyn-volumes #,@crescendo-volumes)))

#;(define-art wolf-strudel
  (loop 160 song (^->note) 

    (abstract (voice@ (melody) volume)
              (voice@ (mid-bass) volume)
              (voice@ (bass) volume)
              (voice@ (accomp) volume))

    (voice@ (melody) (volume 1.2) (instrument "square")) 
    (voice@ (mid-bass) (volume 1) (instrument "square")
            (i@ [0 160] (loop 16 ))) 
    (voice@ (bass) (volume .5) (instrument "square")) 
    (voice@ (accomp) (instrument "supersaw") (volume 1.2))))