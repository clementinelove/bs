#lang info
(define collection "bs")
(define version "1.0")

(define scribblings '(("scribblings/bs.scrbl")))
(define deps '("base"
               "brag"
               "crypto-lib"
               "parser-tools-lib"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     "scribble-lib"))
