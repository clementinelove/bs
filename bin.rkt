#lang racket/base
(require racket/format
         racket/contract
         racket/bool
         openssl/sha1)

(module+ test
  (require rackunit))

(provide
 (contract-out
  [bytes->integer ((bytes? boolean?) (boolean?) . ->* . exact-integer?)]))

;; convert bytes into binary lists
(define (byte->8-bit-string byte)
  (let* ([bin-str (~r (string->number
                       (string-append "#x" (bytes->hex-string (bytes byte)))) #:base 2)]
         [original-len (string-length bin-str)])
    (for/fold ([fstr bin-str])
              ([i (- 8 original-len)])
      (string-append "0" fstr))))

(define (combine-8-bit-strings 8-bit-str-lst #:big-endian? [big-endian? #t] )
  (for/fold ([fstr ""])
            ([new-str (in-list (if big-endian?
                                   8-bit-str-lst
                                   (reverse 8-bit-str-lst)))])
    (string-append fstr new-str)))

(module+ test
  (check-equal? (combine-8-bit-strings (list "00000001" "00001010" "10000000"))
                "000000010000101010000000")
  (check-equal? (combine-8-bit-strings (list "00000001" "00001010" "10000000")
                                       #:big-endian? #f)
                "100000000000101000000001"))

(module+ test
  (check-equal? (byte->8-bit-string 1) "00000001")
  (check-equal? (byte->8-bit-string 10) "00001010")
  (check-equal? (byte->8-bit-string 128) "10000000"))

(define (bytes->integer bs signed? [big-endian? #t])
  (if (= (bytes-length bs) 0)
      0
      (let* ([8-bit-string-lst (map (λ (b) (byte->8-bit-string b)) (bytes->list bs))]
             [combined-str (combine-8-bit-strings 8-bit-string-lst #:big-endian? big-endian?)])
        (if signed?
            (let ([sign (if (char=? (string-ref combined-str 0) #\0) + -)]
                  [rest-str (substring combined-str 1)])
              (sign (string->number (string-append "#b" rest-str))))
            (string->number (string-append "#b" combined-str))))))

(module+ test
  (define delta 0)
  (check-= (bytes->integer (bytes) #t #f) 0 delta)
  (check-= (bytes->integer (bytes 0) #t #f) 0 delta)
  (check-= (bytes->integer (bytes 127) #t #f) 127 delta)
  (check-= (bytes->integer (bytes 128) #t #f) 0 delta)
  (check-= (bytes->integer (bytes 255) #t #f) -127 delta)
  (check-= (bytes->integer (hex-string->bytes "ff8f") #t #f) -4095 delta)
  (check-= (bytes->integer (hex-string->bytes "0090") #t #f) -4096 delta))

;; ---------------------------------------------------------------

(provide
 (contract-out
  [integer->bytes ((exact-integer?) (boolean?) . ->* . bytes?)]))

(define (integer->bytes i [signed? #t])
  (if (= i 0)
      (bytes)
      (let ([sign-bit (if (negative? i) "1" "0")]
            [abs-i (abs i)])
        (list->bytes (map (λ (8-bit-str)
                            (string->number (string-append "#b" 8-bit-str)))
                          (8-bit-string->list (extend-to-8-bit-string
                                               (~r abs-i #:base 2) sign-bit) #f))))))
(module+ test
  (require openssl/sha1)
  (check-equal? (integer->bytes 0) (bytes))
  (check-equal? (integer->bytes -1) (hex-string->bytes "81"))
  (check-equal? (integer->bytes -4095) (hex-string->bytes "ff8f"))
  (check-equal? (integer->bytes -4096) (hex-string->bytes "0090")))

(define (extend-to-8-bit-string bin-str sign-bit)
  (let ([str-len (string-length bin-str)])
    (let-values ([(quot rem) (quotient/remainder str-len 8)])
      (string-append sign-bit (for/fold ([fstr bin-str])
                                        ([i (- 8 rem 1)])
                                (string-append "0" fstr))))))

(define (string-divide str n #:reverse [rev? #f])
  (for/fold ([lst '()]
             #:result (if rev? lst (reverse lst)))
            ([i (/ (string-length str) n)])
    (cons (substring str (* i n) (* (add1 i) n)) lst)))

(module+ test
  (check-equal? (string-divide "1100110011111111" 8) '("11001100" "11111111")))

(define (8-bit-string->list str [big-endian? #t])
  (string-divide str 8 #:reverse (not big-endian?)))

(module+ test
  (check-equal? (8-bit-string->list "1100110011111111") '("11001100" "11111111"))
  (check-equal? (8-bit-string->list "1100110011111111" #f) '("11111111" "11001100")))

