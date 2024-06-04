#lang scribble/acmart @sigplan @review @10pt

@require{references.rkt}
@require[racket/gui]

@include-abstract{abstract.scrbl}
@include-section{introduction.scrbl}
@include-section{demo.scrbl}

@(define em (email "jagen315@gmail.com"))

@title{Demo: Composable Compositions with Tonart}
@author[#:email em]{Jared Gentner}

@; optional: set the author names in the page headers
@elem[#:style "Sshortauthors"]{J. Gentner}