#lang racket
(require parser-tools/lex
         (only-in openssl/sha1
                  hex-string->bytes)
         (prefix-in : parser-tools/lex-sre))

(provide lex token->string)

(define-tokens bs-tokens
  (OPPUSHDATA OPCODE DECIMAL HEXADECIMAL ERROR))

(define-empty-tokens ignored-tokens
  (EOF WHITESPACE COMMENT))

(define lex
  (lexer-src-pos
   ;; whitespace 
   [whitespace 'WHITESPACE]
   ;; pushdata statements
   [(:seq "OP_PUSHDATA" (char-range #\1 #\4))
    (token-OPPUSHDATA (string->symbol lexeme))]
   ;; opcode
   [(:seq "OP"
          #\_
          (:* (:or (char-range #\0 #\9)
                   (char-range #\A #\Z)
                   (char-range #\a #\z))))
    (token-OPCODE (string->symbol lexeme))]
   ;; decimal data
   [(:+ (char-range #\0 #\9))
    (token-DECIMAL (string->number lexeme))]
   ;; hexadecimal data
   [(:seq "0x" (:+ (:or (char-range #\0 #\9)
                        (char-range #\A #\F)
                        (char-range #\a #\f))))
    (token-HEXADECIMAL (substring lexeme 2))]
   ;; single line # comment
   [(:seq #\# (:* (complement #\newline)) #\newline)
    'COMMENT]
   ;; end of file
   [(eof) 'EOF]
   ;; error for any-char
   [any-char (token-ERROR lexeme)]))

(define (token->string t v)
  (if v
      (format "~a" v)
      (format "~a" t)))

(define (tokenize ip)
  ;(port-count-lines! ip)
  (define (next-token)
    (lex ip))
  next-token)