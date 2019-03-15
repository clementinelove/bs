#lang racket
;(require syntax/readerr)
(require "tokenizer.rkt"
         "parser.rkt")

(provide read-syntax)

(define (read-syntax src in)
  (let* ([parse-tree (parse src (tokenize in src))]
         [module-datum `(module bs-mod "expander.rkt"
                          ,parse-tree)])
    ;(printf "~s\n" module-datum)
    (datum->syntax #f module-datum)))

