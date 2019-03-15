#lang racket
(require brag/support)

(provide tokenize)

(define (tokenize ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (define (next-token)
    (let ([bs-lexer
           (lexer-srcloc
            ;; whitespace 
            [whitespace
             (next-token)]
            [(:seq "OP_PUSHDATA" (char-range #\1 #\4))
             (token 'OPPUSHDATA (string->symbol lexeme)
                    #:position (pos lexeme-start)
                    #:line (line lexeme-start)
                    #:column (col lexeme-start)
                    #:span (- (pos lexeme-end)
                              (pos lexeme-start)))]
            ;; opcode
            [(:seq "OP_"
                   (:* (:or (char-range #\0 #\9)
                            (char-range #\A #\Z)
                            (char-range #\a #\z))))
             (token 'OPCODE (string->symbol lexeme)
                    #:position (pos lexeme-start)
                    #:line (line lexeme-start)
                    #:column (col lexeme-start)
                    #:span (- (pos lexeme-end)
                              (pos lexeme-start)))]
            ;; decimal data
            [(:+ (char-range #\0 #\9))
             (token 'DEC (string->number lexeme)
                    #:position (pos lexeme-start)
                    #:line (line lexeme-start)
                    #:column (col lexeme-start)
                    #:span (- (pos lexeme-end)
                              (pos lexeme-start)))]
            ;; hexadecimal data
            [(:seq "0x" (:+ (:or (char-range #\0 #\9)
                                 (char-range #\A #\F)
                                 (char-range #\a #\f))))
             (token 'HEX (substring lexeme 2)
                    #:position (pos lexeme-start)
                    #:line (line lexeme-start)
                    #:column (col lexeme-start)
                    #:span (- (pos lexeme-end)
                              (pos lexeme-start)))]
            ;; single line # comment
            [(:: "#" (:* (:~ #\newline)))
             (next-token)]
            ;; end of file
            [(from/to "<" ">")
             (next-token)]
            #;
            [(eof)
             (void)])])
      (bs-lexer ip)))
  next-token)

#;
(define bs-lexer
  (lexer
   ;; skip whitespaces
   [whitespace (bs-lexer input-port)]

   ;; OP_PUSHDATAn
   #;
   [(concatenation "OP_PUSHDATA"
                   (union (char-range #\0 #\4)))
    (cons `(OP_PUSHDATA ,(string->symbol lexeme)) (bs-lexer input-port))]
   
   ;; op_codes 
   [(concatenation (union #\O #\o)
                   (union #\P #\p)
                   #\_
                   (repetition 1 +inf.0 (union (char-range #\0 #\9)
                                               (char-range #\A #\Z)
                                               (char-range #\a #\z))))
    (cons `(OP_CODE ,(string->symbol lexeme)) (bs-lexer input-port))]

   ;; decimal number
   [(repetition 1 +inf.0 (char-range #\0 #\9))
    (cons `(DEC ,(string->number lexeme)) (bs-lexer input-port))]
   
   ;; hexadecimal number (using bytes representation)
   [(concatenation "0x" (repetition 1 +inf.0 (union (char-range #\0 #\9)
                                                    (char-range #\A #\F)
                                                    (char-range #\a #\f))))
    (cons `(HEX ,(hex-string->bytes (substring lexeme 2)))
          (bs-lexer input-port))]

   ;; c-style comment
   [(concatenation #\/ #\*)
    (comment-lexer input-port)]

   ;; python-style single line comment
   [(concatenation #\# (repetition 1 +inf.0 (complement #\newline)) #\newline)
    (bs-lexer input-port)]
   
   ;; end-of-file
   [(eof) '()]))

#;
(define comment-lexer
  (lexer
   [(concatenation #\* #\/)
    (bs-lexer input-port)]
   [any-char
    (comment-lexer input-port)]))