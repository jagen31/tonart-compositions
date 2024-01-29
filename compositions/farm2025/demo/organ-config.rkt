#lang racket

(require tonart)
(provide (all-defined-out) (for-syntax (all-defined-out)))

(define-art man-conf
    (manual-channel-mapping {I 1} {II 2} {III 3} {P 9}))

(define-art stop-conf
    (stop-data-config
        {I
            [flute-16 1] [montre 2] [harm-flute 3] [flute 4] [gamba 5]
            [prestant 6] [flute-4 7]
            [plein-jeu 10] [trumpet 11] [clarion 12]}
        {II
            [cor-de-nuit 21]
            [salicional 22]
            [unda-maris 23]
            [clarinet 24]
            [string-4 26]
            [flute-2 28]
            [krumhorn 30]
            }
        {III 
            [flute 41]
            [bourdon 42]
            [aeoline 43]
            [flute-4 45]
            [fugara 46]
            [harmonia 48]
            [bassoon 49]
            [trumpet 50]
            [oboe 51]
            [clarion 52]}
        {P [contrabass 61]
           [subbass 62]
           [octave 63]
           [cello 64]
           [bombarde 66]}
        (Aux [III/II 81] [III/I 82] [II/I 83])))

(define-art piston-conf
    (piston-data-config [gc 90]))

(define-art reg
    (registration {I gamba}
                  {II salicional}
                  {III}
                  {P cello subbass}
                  {Aux III/II}))

(define-art reg2
    (registration {I gamba flute-4}
                  {II salicional unda-maris}
                  {III flute bourdon aeoline}
                  {P cello octave subbass}
                  {Aux III/II}))
        
(define-art reg3
    (registration {I flute-16 gamba flute-4 montre}
                  {II salicional unda-maris}
                  {III flute bourdon aeoline fugara}
                  {P cello octave subbass contrabass}
                  {Aux III/II}))

(define-art reg4
    (registration {I flute-16 montre plein-jeu}
                  {II}
                  {III bourdon aeoline fugara harmonia bassoon trumpet}
                  {P cello octave subbass contrabass}
                  {Aux III/II}))
