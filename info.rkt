#lang info
(define collection "bs")
(define version "1.0")

(define scribblings '(("scribblings/bs.scrbl")))

(define deps '("syntax-color-lib"
               "base"
               "brag"
               "crypto-lib"
               "parser-tools-lib"))
(define build-deps '("drracket-core"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-lib"))
