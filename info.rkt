#lang info
(define collection "bs")
(define version "1.0")

(define scribblings '(("scribblings/bs.scrbl" (multi-page))))
(define deps '("base"
               "brag"
               "crypto-lib"
               "parser-tools-lib"
               "syntax-color-lib"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     "scribble-lib"))
