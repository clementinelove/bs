#lang racket/base

(module+ test
  (require rackunit))

;; =============================
;; List Helper Functions
;; =============================

(module list-helpers racket/base
  (require racket/contract)
  (provide (contract-out
            [list-n-removed (list? natural-number/c . -> . list?)] ;; TODO: check size difference
            [list-n-removed/removed-elem (list? natural-number/c . -> . (values list? any/c))]))

  (define (list-n-removed lst n)
    (cond [(null? lst) (error 'list-remove-n "given list should at least have ~s elements" n)]
          [(= n 0) (cdr lst)]
          [else (cons (car lst) (list-n-removed (cdr lst) (sub1 n)))]))

  (define (list-n-removed/removed-elem lst n)
    (cond [(null? lst) (error 'list-remove-n "given list should at least have ~s elements" n)]
          [(= n 0) (values (cdr lst) (car lst))]
          [else (let-values ([(list-n-removed removed-elem)
                              (list-n-removed/removed-elem (cdr lst) (sub1 n))])
                  (values (cons (car lst) list-n-removed) removed-elem))])))

(require 'list-helpers)

(module+ test
  (check-equal? (list-n-removed '(1 2 3) 0) '(2 3))
  (check-equal? (list-n-removed '(1 2 3) 2) '(1 2))
  (check-equal? (list-n-removed '(1) 0) '())

  (call-with-values (λ () (list-n-removed/removed-elem '(1 2 3) 0))
                    (λ (lst elem)
                      (check-= 1 elem 0)))
  (call-with-values (λ () (list-n-removed/removed-elem '(1 2 3) 2))
                    (λ (lst elem)
                      (check-= 3 elem 0)))
  (call-with-values (λ () (list-n-removed/removed-elem '(1) 0))
                    (lambda (lst elem)
                      (check-= 1 elem 0))))

;; =============================
;; Predicates
;; =============================

(module predicates racket/base
  (require racket/contract)
  (provide (contract-out
            [hex-string? (any/c . -> . boolean?)]))

  (define (hex-string? v)
    (and (string? v)
         (regexp-match? #px"^([[:xdigit:]]{2})*$" v))))

(require 'predicates)

;; =============================
;; Data Coverter
;; =============================

(module converters racket/base
  (require racket/contract
           (submod ".." predicates)
           (only-in openssl/sha1
                    hex-string->bytes
                    bytes->hex-string))

  (provide (contract-out
            [bytes->boolean (bytes? . -> . boolean?)]
            [boolean->bytes (boolean? . -> . bytes?)]

            [integer->boolean (exact-integer? . -> . boolean?)]
            [boolean->integer (boolean? . -> . exact-integer?)]

            [hex-string->bytes (hex-string? . -> . bytes?)]
            [bytes->hex-string (bytes? . -> . hex-string?)]

            [bytes->integer ((bytes? boolean?) (boolean?) . ->* . exact-integer?)]
            [integer->bytes ((exact-integer?) (boolean?) . ->* . bytes?)]))

  ;; convert a single byte string bs to a boolean value
  ;; return false if bs's interpreted integer value is 0
  ;; else return true
  (define (bytes->boolean bs)
    (not (= (bytes->integer bs #t #f) 0)))

  (define TRUE 1)
  (define FALSE 0)

  (define (boolean->bytes bool)
    (integer->bytes (if bool TRUE FALSE)))

  (define (integer->boolean i)
    (not (= i 0)))

  (define (boolean->integer b)
    (if b TRUE FALSE))

  ;; -----------------------------------------------------------
  ;; integer <-> bytes
  (require racket/format)

  ;; convert bytes into binary lists
  (define (byte->8-bit-string byte)
    (let* ([bin-str (~r (string->number
                         (string-append "#x" (bytes->hex-string (bytes byte)))) #:base 2)]
           [original-len (string-length bin-str)])
      (for/fold ([fstr bin-str])
                ([i (- 8 original-len)])
        (string-append "0" fstr))))

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


  ;; example:
  ;;     (combine-8-bit-strings (list "00000001" "00001010" "10000000"))
  ;;  => "000000010000101010000000"
  ;; d
  ;;     (combine-8-bit-strings (list "00000001" "00001010" "10000000")
  ;;                            #:big-endian? #f)
  ;;  => "100000000000101000000001")
  (define (combine-8-bit-strings 8-bit-str-lst #:big-endian? [big-endian? #t] )
    (for/fold ([fstr ""])
              ([new-str (in-list (if big-endian?
                                     8-bit-str-lst
                                     (reverse 8-bit-str-lst)))])
      (string-append fstr new-str)))

  ;; convert integer to bytes
  (define (integer->bytes i [signed? #t])
    (if (= i 0)
        (bytes)
        (let ([sign-bit (if (negative? i) "1" "0")]
              [abs-i (abs i)])
          (list->bytes (map (λ (8-bit-str)
                              (string->number (string-append "#b" 8-bit-str)))
                            (8-bit-string->list (extend-to-8-bit-string
                                                 (~r abs-i #:base 2) sign-bit) #f))))))

  ;; extend a binary string to 8-bit string format, with a sign bit.
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

  (define (8-bit-string->list str [big-endian? #t])
    (string-divide str 8 #:reverse (not big-endian?)))
  )

(require 'converters)

(module+ test
  (define delta 0)
  (check-= (bytes->integer (bytes) #t #f) 0 delta)
  (check-= (bytes->integer (bytes 0) #t #f) 0 delta)
  (check-= (bytes->integer (bytes 127) #t #f) 127 delta)
  (check-= (bytes->integer (bytes 128) #t #f) 0 delta)
  (check-= (bytes->integer (bytes 255) #t #f) -127 delta)
  (check-= (bytes->integer (hex-string->bytes "ff8f") #t #f) -4095 delta)
  (check-= (bytes->integer (hex-string->bytes "0090") #t #f) -4096 delta))

(module+ test
  (require openssl/sha1)
  (check-equal? (integer->bytes 0) (bytes))
  (check-equal? (integer->bytes -1) (hex-string->bytes "81"))
  (check-equal? (integer->bytes -4095) (hex-string->bytes "ff8f"))
  (check-equal? (integer->bytes -4096) (hex-string->bytes "0090")))

;; =============================
;; Crypto Related
;; =============================
(module cryptographic racket/base
  (require racket/contract
           crypto
           crypto/libcrypto)

  (provide (contract-out
            [ripemd160 (bytes? . -> . bytes?)]
            [sha1 (bytes? . -> . bytes?)]
            [sha256 (bytes? . -> . bytes?)]
            [hash160 (bytes? . -> . bytes?)]
            [hash256 (bytes? . -> . bytes?)]))

  ;; configure the 'search path' of crypto factories
  (crypto-factories (list libcrypto-factory))

  (define (ripemd160 bs)
    (digest 'ripemd160 bs))

  (define (sha1 bs)
    (digest 'sha1 bs))

  (define (sha256 bs)
    (digest 'sha256 bs))

  (define (hash160 bs)
    (ripemd160 (sha256 bs)))

  (define (hash256 bs)
    (sha256 (sha256 bs)))
  )

(require 'cryptographic)

;; =============================
;; Other Requires / Last Provide
;; =============================


(provide (all-from-out 'predicates)
         (all-from-out 'converters)
         (all-from-out 'cryptographic)
         (all-from-out 'list-helpers))
