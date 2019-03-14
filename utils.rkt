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
;; Data Structures
;; =============================

(module data-structures racket/base
  (require racket/contract
           racket/list
           (submod ".." list-helpers))

  (provide
   (contract-out
    ;; s-machine
    [struct s-machine ((main-stk stack?)
                       (alt-stk stack?)
                       (tran-state boolean?)
                       (level (listof boolean?)))]
    ;[s-machine? (any/c . -> . boolean?)]
    ;; stack
    [stack? (any/c . -> . boolean?)]
    [empty-stack (-> stack?)]
    [list->stack (list? . -> . stack?)]
    [stack->list (stack? . -> . list?)]
    [stack-empty? (stack? . -> . boolean?)]
    [stack-length (stack? . -> . natural-number/c)]

    [top (stack? . -> . any/c)]

    [pop (stack? . -> . stack?)]
    [pop-nip (stack? . -> . stack?)]

    [rotate (stack? . -> . stack?)]
    [swap (stack? . -> . stack?)]

    [push (stack? any/c . -> . stack?)]
    [push-dup-n (stack? exact-positive-integer? . -> . stack?)]
    [push-pick (stack? natural-number/c . -> . stack?)]
    [push-tuck (stack? . -> . stack?)]

    [roll (stack? natural-number/c . -> . stack?)]))

  ;; A script machine is a data structure that stores the states
  ;; of current running script program.
  ;; main-stk : the main stack
  ;; alt-stk : the alt stack
  ;; tran-state : transaction state, where #f means the transaction is invalid
  ;; level : a list for storing level branching information
  ;;         each element is a skipping boolean:
  ;;           #t -> skip next token (except (skipping-executable? token) => #t)
  ;;           #f -> do not skip next token
  ;; length of the level decides the block level
  (struct s-machine (main-stk alt-stk tran-state level))

  ;; Stack
  (struct stack (contents) #:transparent)

  ;; constructors
  (define (empty-stack)
    (stack '()))

  (define (list->stack lst)
    (stack lst))

  ;; predicates
  (define (stack-empty? stk)
    (null? (stack-contents stk)))

  ;; converters
  (define (stack->list stk)
    (stack-contents stk))

  ;; getters
  (define (stack-length stk)
    (length (stack-contents stk)))

  (define (top stk)
    (car (stack-contents stk)))

  (define (pop stk)
    (if (stack-empty? stk)
        (error 'pop "stack is empty")
        (struct-copy stack stk
                     [contents (rest (stack-contents stk))])))

  (define (pop-nip stk)
    (if (stack-empty? stk)
        (error 'pop-nip "stack is empty")
        (let ([contents (stack-contents stk)])
          (struct-copy stack stk
                       [contents (cons (first contents) (cddr contents))]))))
  #;
  (define (rotate stk)
    (if (stack-empty? stk)
        (error 'pop-nip "stack is empty")
        (let ([old-contents (stack-contents stk)]
              [rotate-3 (λ (lst)
                          (cons (third lst) (take lst 2)))])
          (let-values ([(first-3 rest-c) (split-at old-contents 3)])
            (struct-copy stack stk
                         [contents (append (rotate-3 first-3) rest-c)])))))

  (define (rotate stk)
    (let* ([old-contents (stack-contents stk)]
           [1st (first old-contents)]
           [2nd (second old-contents)]
           [3rd (third old-contents)]
           [drop-three (list-tail old-contents 3)])
      (list->stack (cons 3rd
                         (cons 1st
                               (cons 2nd drop-three))))))
  
  (define (swap stk)
    (if (< (stack-length stk) 2)
        (error 'pop-nip "stack should at least have 2 elements")
        (let ([old-contents (stack-contents stk)]
              [swap (λ (lst)
                      (cons (second lst)
                            (cons (first lst)
                                  (cddr lst))))])
          (struct-copy stack stk
                       [contents (swap old-contents)]))))

  (define (push stk val)
    (struct-copy stack stk
                 [contents (cons val (stack-contents stk))]))

  (define (push-dup-n stk n)
    (let* ([old-contents (stack-contents stk)]
           [first-n (take old-contents n)])
      (list->stack (append first-n old-contents))))

  (define (push-pick stk i)
    (if (< (stack-length stk) i)
        (error 'pop-nip "stack should at least have ~s elements" i)
        (let* ([old-contents (stack-contents stk)]
               [i-th-item (list-ref old-contents i)])
          (push stk i-th-item))))

  (define (push-tuck stk)
    (let* ([old-contents (stack-contents stk)]
           [first-v (first old-contents)]
           [second-v (second old-contents)]
           [drop-two (list-tail old-contents 2)])
      (list->stack (cons first-v
                         (cons second-v
                               (cons first-v drop-two))))))  
  
  ;; TODO: better error handling
  (define (roll stk i)
    (let-values ([(n-removed-lst elem) (list-n-removed/removed-elem (stack->list stk) i)])
      (push (list->stack n-removed-lst) elem))))

(module+ test
  (check-equal? (empty-stack) (list->stack '()))
  (check-equal? (push-dup-n (list->stack '(5 4 3 2 1)) 3) (list->stack '(5 4 3 5 4 3 2 1)))
  (check-equal? (push-tuck (list->stack '(1 2 3 4)))
                (list->stack '(1 2 1 3 4)))
  (check-equal? (roll (list->stack '(0 1 2 9)) 2)
                (list->stack '(2 0 1 9))))

(require 'data-structures)

;; =============================
;; Other Requires / Last Provide
;; =============================


(provide (all-from-out 'predicates)
         (all-from-out 'converters)
         (all-from-out 'cryptographic)
         (all-from-out 'data-structures))
