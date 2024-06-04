#lang scribble/acmart
@require{references.rkt}
@require[scriblib/footnote]
@require[(except-in scribble/manual index)
         scribble-abbrevs/manual (for-label tonart)
         (except-in scribble/eval examples)
         scribble/example
         scribble/bnf]

@title{Introduction}
 
@(define helper-eval (make-base-eval))
@interaction-eval[#:eval helper-eval
                  (require art/sequence/ravel tonart 2htdp/image)]


@section{Introduction}

We begin this demo with Tonart's syntax.  For the purpose of simplicity, the
extension points have been elided, and a fixed set of forms is provided for
each nonterminal.

@subsection{Tonart Syntax}

  Simplified Tonart syntax is as follows:

  @linebreak[]

  @(let ([open @litchar{(}]

         [close @litchar{)}])

     @BNF[(list @nonterm{form}
                @nonterm{id}
                  @nonterm{object}
                    @nonterm{rewriter}
                    @nonterm{context}
                @BNF-seq-lines[
                  (list @BNF-seq[open @code{@"@"} @BNF-seq[open @kleenestar[@nonterm{coord}] close]])
                  (list @hspace[1] @BNF-seq[@kleenestar[@nonterm{art-form}] close])])

          (list @nonterm{program}
                @BNF-seq[open @code{define-art} @nonterm{art-id} @kleenestar[@nonterm{art-form}] close]
                @BNF-seq-lines[
                  (list @BNF-seq[open @code{realize} @nonterm{art-realizer} close])
                  (list @hspace[1] @kleenestar[@nonterm{art-form}] close)])])


  @(let ([open @litchar{(}]

         [close @litchar{)}])

          (list @nonterm{object}
                @BNF-seq[open @code{note} @nonterm{pitch} @nonterm{accidental} @nonterm{octave} close]
                @BNF-seq[open @code{tone} @nonterm{frequency} close]
                @BNF-seq[open @code{chord} @nonterm{pitch} @nonterm{accidental} @nonterm{quality} close]
                @BNF-seq[open @code{rhythm} @kleenestar[@nonterm{number}] close]))

  @(let ([open @litchar{(}]

         [close @litchar{)}])

          (list @nonterm{rewriter}
                @BNF-seq[open @code{chord->notes} close]
                @BNF-seq[open @code{note->tone} close]
                @BNF-seq[open @code{apply-rhythm} close]))

  @(let ([open @litchar{(}]

         [close @litchar{)}])

          (list @nonterm{context}
                @BNF-seq[open @code{music} @kleenestar[@nonterm{form}] close]
                @BNF-seq[open @code{seq} @kleenestar[@nonterm{form}] close])

          (list @nonterm{coord}
                @BNF-seq[open @code{interval} @nonterm{number} @nonterm{number} close]
                @BNF-seq[open @code{voice} @kleenestar[@nonterm{id}] close]
                @BNF-seq[open @code{index} @kleenestar[@nonterm{number}] close]))
        