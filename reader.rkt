#lang racket
;(require syntax/readerr)
(require bs/tokenizer
         bs/parser)

(provide read-syntax)

(define (read-syntax src in)
  (let* ([parse-tree (parse src (tokenize in src))]
         [module-datum `(module bs-mod bs/expander
                          ,parse-tree)])
    ;(printf "~s\n" module-datum)
    (datum->syntax #f module-datum)))

