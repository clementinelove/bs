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