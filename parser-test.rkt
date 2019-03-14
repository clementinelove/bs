#lang racket/base
(require "parser.rkt" "tokenizer.rkt"
         brag/support)

(parse-to-datum (apply-tokenizer-maker tokenize "2 0x01"))