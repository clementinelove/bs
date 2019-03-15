#lang racket/base
(module reader racket/base
  (require "reader.rkt")
  (provide read-syntax)

  (provide get-info)
  (define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        [(color-lexer)
         (dynamic-require 'bs/colorer 'color-bs)]
        [else default]))
    handle-query))