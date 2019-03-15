#lang racket/base
(require brag/support)

(define bs-lexer
  (lexer
   [(eof) (values lexeme 'eof #f #f #f)]
   #;
   [whitespace
    (values lexeme 'white-space #f #f #f)]

   [(:seq "OP_" (:+ (:or alphabetic numeric)))
    (values lexeme 'symbol #f
            (pos lexeme-start) (pos lexeme-end))]

   [(:+ numeric)
    (values lexeme 'symbol #f
            (pos lexeme-start) (pos lexeme-end))]

   [(:seq "0x" (:+ (:or numeric (char-range "A" "F") (char-range "a" "f"))))
    (values lexeme 'constant #f
            (pos lexeme-start) (pos lexeme-end))]

   ["0x"
    (values lexeme 'error #f
            (pos lexeme-start) (pos lexeme-end))]
   
   [(:: "#" (:* (:~ #\newline)))
    (values lexeme 'comment #f
            (pos lexeme-start) (pos lexeme-end))]

   [(from/to "<" ">")
    (values lexeme 'comment
            (if (equal? lexeme "<")
                '|(|
                '|)|)
            (pos lexeme-start) (pos lexeme-end))]
   
   [any-char
    (values lexeme 'symbol #f
            (pos lexeme-start) (pos lexeme-end))]))

(define (color-bs port offset racket-coloring-mode?)
  (define-values (str cat paren start end)
    (bs-lexer port))
  (values str cat paren start end 0 #f)
  #;
  (cond [(or (not racket-coloring-mode?) (equal? (peek-string 2 0 port) "0x"))
         (define-values (str cat paren start end)
           (bs-lexer port))
         (define switch-to-racket-mode (regexp-match #px"\\s" str))
         (values str cat paren start end 0 switch-to-racket-mode)]
        [else
         (define-values (str cat paren start end)
           (racket-lexer port))
         (values str cat paren start end 0 #t)]))

(provide color-bs)
