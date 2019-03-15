#lang racket/base
(require racket/port
         bs/parser
         bs/tokenizer)
(provide bs-output-port do-setup!)

(define bs-output-port
  (make-parameter (open-output-nowhere)))

(define repl-parse (make-rule-parser bs-program))

(define (read-one-line origin port)
  (define one-line (read-line port))
  (if (eof-object? one-line)
      eof
      (repl-parse
       (tokenize (open-input-string one-line)))))

(define (do-setup!)
  (bs-output-port (current-output-port))
  (current-read-interaction read-one-line))
