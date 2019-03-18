#lang racket/base
(require brag/support)

(define-lex-abbrevs
  [digit (:/ "0" "9")]
  [digits (:+ digit)]
  [hex-char (:/ "af" "AF" "09")]
  [valid-hex-str (:: "0x" (:+ (:: hex-char hex-char)))]
  [all-hex-str (:: "0x" (:* hex-char))]
  [invalid-hex-str (intersection all-hex-str (complement valid-hex-str))]
  [identifier-chars (:or "_" (:/ "AZ" "09"))]
  [identifier (:: "OP_" (:* identifier-chars))]
  [single-line-comment (:: "#" (:* (:~ #\newline)))]
  [multi-line-comment (:: "<" (complement (:: any-string ">" any-string)) ">")])

(define bs-lexer
  (lexer
   [(eof) (values lexeme 'eof #f #f #f)]

   ["OP_1"
    (values lexeme 'symbol #f
            (pos lexeme-start) (pos lexeme-end))] 
   #;
   [digits
    (values lexeme 'constant #f
            (pos lexeme-start) (pos lexeme-end))]

   [invalid-hex-str
    (values lexeme 'error #f
            (pos lexeme-start) (pos lexeme-end))]

   [valid-hex-str
    (values lexeme 'constant #f
            (pos lexeme-start) (pos lexeme-end))]
     
   [single-line-comment
    (values lexeme 'comment #f
            (pos lexeme-start) (pos lexeme-end))]

   [(:: "<" (:* (:~ ">"))) ;; multi line comment
    (values lexeme 'comment #f
            (pos lexeme-start) (pos lexeme-end))]

   [multi-line-comment
    (values lexeme 'comment
            (if (equal? lexeme "<") '|(| '|)|)
            (pos lexeme-start) (pos lexeme-end))]
   
   [any-char
    (values lexeme 'symbol #f
            (pos lexeme-start) (pos lexeme-end))]))

(define (color-bs port offset racket-coloring-mode?)
  (define-values (str cat paren start end)
    (bs-lexer port))
  (values str cat paren start end 0 #f))

(provide color-bs)
