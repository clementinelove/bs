#lang racket/base
(require bs/parser bs/tokenizer
         brag/support)

;(parse-to-datum (apply-tokenizer-maker tokenize "2 0x01"))

(define source (open-input-string "OP_1 2 0x0102"))

(define exp-parser (make-rule-parser bs-program))

(define exp-parse-tree (exp-parser (tokenize source)))
(syntax->datum exp-parse-tree)