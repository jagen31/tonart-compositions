#lang scribble/acmart @sigplan @10pt

@require{references.rkt}
@require[racket/gui]

@include-abstract{abstract.scrbl}
@include-section{demo.scrbl}

@(define em (email "jagen315@gmail.com"))

@title{Demo: Composable Compositions with Tonart}
@author[#:email em #:affiliation (affiliation #:city "Boston" #:state "MA" #:country "USA")]{Jared Gentner}

@; optional: set the author names in the page headers
@elem[#:style "Sshortauthors"]{J. Gentner}

@(acmConference "FARM '24" "September 2, 2024" "Milan, Italy")
@(acmDOI "10.1145/3677996.3678294")
@(acmISBN "")

@generate-bibliography[#:sec-title "References"]
