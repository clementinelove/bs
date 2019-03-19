#lang racket/base
(require racket/match
         bs/utils
         bs/structs
         racket/contract
         racket/bool)

;; modify this template for defining a new procedure
#;
(define (OP_TEMPLATE stk alt-stk tran-state level)
  (values stk
          alt-stk
          tran-state
          level))


(provide (all-defined-out))
;; Test if a symbol refers to a special opcode.
;; Return the data bytes to be pushed if op-sym is a special opcode.
;; Otherwise return #f
(define (special-op? op-sym)
  (match op-sym
    ['OP_PUSHDATA1 1]
    ['OP_PUSHDATA2 2]
    ['OP_PUSHDATA3 3]
    ['OP_PUSHDATA4 4]
    [else #f]))

;; =============================
;; Common Abstraction facilities
;; =============================

(define (OP_N sm n)
  (let ([main-stk (s-machine-main-stk sm)])
    (struct-copy s-machine sm [main-stk (push main-stk (integer->bytes n))])))

(define (OP_UNARY #:name [name 'OP_UNARY]
                  proc sm)
  (let ([stk (s-machine-main-stk sm)])
    (if (stack-empty? stk)
        (error name "main stack should at least have 1 element to perform this procedure")
        (struct-copy s-machine sm [main-stk (push (pop stk) (proc (top stk)))]))))

(define (OP_BINARY #:name [name 'OP_BINARY]
                   proc sm)
  (let ([stk (s-machine-main-stk sm)])
    (if (< (stack-length stk) 2)
        (error name "main stack should at least have 2 element to perform this procedure")
        (let ([remove-top-stk (pop stk)])
          (struct-copy s-machine sm [main-stk (push (pop remove-top-stk)
                                                    (proc (top stk) (top remove-top-stk)))])))))

(define (OP_TERNARY #:name [name 'OP_TERNARY]
                    proc sm)
  (let ([stk (s-machine-main-stk sm)])
    (if (< (stack-length stk) 3)
        (error name "main stack should at least have 3 element to perform this procedure")
        (let* ([drop-one (pop stk)]
               [drop-two (pop drop-one)])
          (struct-copy s-machine sm
                       [main-stk (push (pop drop-two)
                                       (proc (top stk)
                                             (top drop-one)
                                             (top drop-two)))])))))

#;
(define (OP_TERNARY proc stk)
  )

;; ======================
;; Constants
;; ======================


(define (OP_0 sm)
  (OP_N sm 0))

(define (OP_FALSE sm)
  (OP_0 sm))

(define (OP_1NEGATE sm)
  (OP_N sm -1))

(define (OP_1 sm)
  (OP_N sm 1))

(define (OP_TRUE sm)
  (OP_1 sm))

(define (OP_2 sm)
  (OP_N sm 2))

(define (OP_3 sm)
  (OP_N sm 3))

(define (OP_4 sm)
  (OP_N sm 4))

(define (OP_5 sm)
  (OP_N sm 5))

(define (OP_6 sm)
  (OP_N sm 6))

(define (OP_7 sm)
  (OP_N sm 7))

(define (OP_8 sm)
  (OP_N sm 8))

(define (OP_9 sm)
  (OP_N sm 9))

(define (OP_10 sm)
  (OP_N sm 10))

(define (OP_11 sm)
  (OP_N sm 11))

(define (OP_12 sm)
  (OP_N sm 12))

(define (OP_13 sm)
  (OP_N sm 13))

(define (OP_14 sm)
  (OP_N sm 14))

(define (OP_15 sm)
  (OP_N sm 15))

(define (OP_16 sm)
  (OP_N sm 16))

;; ============
;; Flow Control
;; ============

(define (skipping-executable? v)
  (list? (member v (list OP_ELSE OP_ENDIF OP_VERIF OP_VERNOTIF))))

;; does nothing
(define (OP_NOP sm)
  sm)

(define (OP_IF sm)
  (let ([stk (s-machine-main-stk sm)]
        [lv (s-machine-level sm)])
    (if (stack-empty? stk)
        (error 'OP_IF "main stack is empty")
        (struct-copy s-machine sm [main-stk (pop stk)]
                     [level (cons (not (bytes->boolean (top stk))) lv)]))))

(define (OP_NOTIF sm)
  (let ([stk (s-machine-main-stk sm)]
        [lv (s-machine-level sm)])
    (if (stack-empty? stk)
        (error 'OP_NOTIF "main stack is empty")
        (struct-copy s-machine sm [main-stk (pop stk)]
                     [level (cons (bytes->boolean (top stk)) lv)]))))

(define (OP_ELSE sm)
  (let ([stk (s-machine-main-stk sm)]
        [lv (s-machine-level sm)])
    (if (null? lv)
        (error 'OP_ELSE "command can only be used inside an OP_IF--OP_ENDIF block.")
        (struct-copy s-machine sm [level (cons (not (car lv)) (cdr lv))]))))

(define (OP_ENDIF sm)
  (let ([stk (s-machine-main-stk sm)]
        [lv (s-machine-level sm)])
    (if (null? lv)
        (error 'OP_ENDIF "used before any OP_IF or OP_NOTIF command")
        (struct-copy s-machine sm [level (cdr lv)]))))

;; mark the transaction as invalid if top stack value is not true.
;; the top stack value is removed.

(define (OP_VERIFY sm)
  (let ([stk (s-machine-main-stk sm)])
    (if (stack-empty? stk)
        (error 'OP_VERIFY "main stack is empty")
        (struct-copy s-machine sm
                     [main-stk (pop stk)]
                     [tran-state (bytes->boolean (top stk))]))))

;; marks transaction as invalid.
(define (OP_RETURN sm)
  (struct-copy s-machine sm [tran-state #f]))

;; =====
;; Stack
;; =====

(define (OP_TOALTSTACK sm)
  (let* ([main-stk (s-machine-main-stk sm)]
         [alt-stk (s-machine-alt-stk sm)]
         [main-stk-top (top main-stk)])
    (struct-copy s-machine sm
                 [main-stk (pop main-stk)]
                 [alt-stk (push alt-stk main-stk-top)])))

(define (OP_FROMALTSTACK sm)
  (let* ([main-stk (s-machine-main-stk sm)]
         [alt-stk (s-machine-alt-stk sm)]
         [alt-stk-top (top alt-stk)])
    (struct-copy s-machine sm
                 [main-stk (push main-stk alt-stk-top)]
                 [alt-stk (pop alt-stk)])))

(define (OP_IFDUP sm)
  (let* ([main-stk (s-machine-main-stk sm)]
         [main-stk-top (top main-stk)])
    (if (= (bytes->integer main-stk-top #t #f) 0)
        sm
        (struct-copy s-machine sm
                     [main-stk (push main-stk main-stk-top)]))))

(define (OP_DEPTH sm)
  (OP_N sm (stack-length (s-machine-main-stk sm))))

(define (OP_DROP sm)
  (struct-copy s-machine sm [main-stk (pop (s-machine-main-stk sm))]))

(define (OP_DUPN sm n)
  (let* ([main-stk (s-machine-main-stk sm)]
         [main-stk-top (top main-stk)])
    (struct-copy s-machine sm
                 [main-stk (push main-stk main-stk-top)])))

(define (OP_DUP sm)
  (let* ([main-stk (s-machine-main-stk sm)]
         [main-stk-top (top main-stk)])
    (struct-copy s-machine sm
                 [main-stk (push main-stk main-stk-top)])))

(define (OP_NIP sm)
  (struct-copy s-machine sm
               [main-stk (pop-nip (s-machine-main-stk sm))]))

(define (OP_OVER sm)
  (let ([main-stk (s-machine-main-stk sm)])
    (struct-copy s-machine sm
                 [main-stk (push-pick main-stk 2)])))

;; Convert the top stack item into an integer n and pop it,
;; then fetch the nth item on the main stack.
;; EXAMPLE: OP_7 OP_8 OP_9 OP_10 OP_1 OP_PICK
;;          Result stack:
;;               --- STACK TOP ---
;;               0x09
;;               0x0a
;;               0x09
;;               0x08
;;               0x07
;;               --- STACK BOT ---
(define (OP_PICK sm)
  (let ([main-stk (s-machine-main-stk sm)])
    (struct-copy s-machine sm
                 [main-stk (push-pick (pop main-stk) (bytes->integer (top main-stk) #t #f))])))

(define (OP_ROLL sm)
  (let ([main-stk (s-machine-main-stk sm)])
    (struct-copy s-machine sm
                 [main-stk (roll (pop main-stk) (bytes->integer (top main-stk) #t #f))])))

(define (OP_ROT sm)
  (let ([main-stk (s-machine-main-stk sm)])
    (struct-copy s-machine sm
                 [main-stk (roll main-stk)])))

(define (OP_SWAP sm)
  (let ([main-stk (s-machine-main-stk sm)])
    (struct-copy s-machine sm
                 [main-stk (swap main-stk)])))

(define (OP_TUCK sm)
  (let ([main-stk (s-machine-main-stk sm)])
    (struct-copy s-machine sm
                 [main-stk (push-tuck main-stk)])))

(define (OP_2DROP sm)
  (let ([main-stk (s-machine-main-stk sm)])
    (struct-copy s-machine sm
                 [main-stk (pop (pop main-stk))])))

(define (OP_2DUP sm)
  (let ([main-stk (s-machine-main-stk sm)])
    (struct-copy s-machine sm
                 [main-stk (push-dup-n main-stk 2)])))

(define (OP_3DUP sm)
  (let ([main-stk (s-machine-main-stk sm)])
    (struct-copy s-machine sm
                 [main-stk (push-dup-n main-stk 3)])))

;; At least 4 item on stack
(define (OP_2OVER sm)
  (let ([main-stk (s-machine-main-stk sm)])
    (struct-copy s-machine sm
                 [main-stk (push-pick (push-pick main-stk 3) 3)])))

(define (OP_2ROT sm)
  (let ([main-stk (s-machine-main-stk sm)])
    (struct-copy s-machine sm
                 [main-stk (roll (roll main-stk 5) 5)])))

(define (OP_2SWAP sm)
  (let ([main-stk (s-machine-main-stk sm)])
    (struct-copy s-machine sm
                 [main-stk (roll (roll main-stk 3) 3)])))

;; ======
;; Splice
;; ======

; Pushes the string length of the top element of the stack (without popping it).
(define (OP_SIZE sm)
  (let ([stk (s-machine-main-stk sm)])
    (struct-copy s-machine sm [main-stk (push stk (bytes-length (top stk)))])))

;; =============
;; Bitwise logic
;; =============

(define (OP_EQUAL sm)
  (OP_BINARY #:name 'OP_EQUAL
             (lambda (top second)
               (boolean->bytes (equal? top second))) sm))

;; TODO: check the implementation of OP_EQUALVERIFY

(define (OP_EQUALVERIFY sm)
  (let ([with-verify-name (λ (sm)
                            (OP_BINARY #:name 'OP_EQUALVERIFY
                                       (lambda (top second)
                                         (boolean->bytes (equal? top second))) sm))])
    ((compose OP_VERIFY with-verify-name) sm)))

;; ===========
;; Arithmetics
;; ===========

;; convert the top level element to integer first
;; then convert back to bytes and put onto the stack
(define (OP_UNARY_ARITH #:name [name 'OP_UNARY_ARITH]
                        op-proc sm)
  (OP_UNARY #:name name
            (λ (top-item)
              (integer->bytes (op-proc (bytes->integer top-item #t #f))))
            sm))

(define (OP_BINARY_ARITH #:name [name 'OP_BINARY_ARITH]
                         op-proc sm)
  (OP_BINARY #:name name
             (λ (top-item second-item)
               (integer->bytes (op-proc (bytes->integer top-item #t #f)
                                        (bytes->integer second-item #t #f))))
             sm))

(define (OP_TERNARY_ARITH #:name [name 'OP_TERNARY_ARITH]
                          op-proc sm)
  (OP_TERNARY #:name name
              (λ (top-item second-item third-item)
                (integer->bytes (op-proc (bytes->integer top-item #t #f)
                                         (bytes->integer second-item #t #f)
                                         (bytes->integer third-item #t #f))))
              sm))

(define (OP_1ADD sm)
  (OP_UNARY_ARITH #:name 'OP_1ADD add1 sm))

(define (OP_1SUB sm)
  (OP_UNARY_ARITH #:name 'OP_1SUB sub1 sm))

(define (OP_NEGATE sm)
  (OP_UNARY_ARITH #:name 'OP_NEGATE - sm))

(define (OP_ABS sm)
  (OP_UNARY_ARITH #:name 'OP_ABS abs sm))

(define (OP_NOT sm)
  (let ([flip
         (λ (n)
           (cond [(= n 0) 1]
                 [(= n 1) 0]
                 [else 0]))])
    (OP_UNARY_ARITH #:name 'OP_NOT flip sm)))

(define (OP_0NOTEQUAL sm)
  (let ([not-equal-to-0
         (λ (n)
           (if (= n 0)
               0
               1))])
    (OP_UNARY_ARITH #:name 'OP_0NOTEQUAL not-equal-to-0 sm)))

(define (OP_ADD sm)
  (OP_BINARY_ARITH #:name 'OP_ADD + sm))

(define (OP_SUB sm)
  (OP_BINARY_ARITH #:name 'OP_SUB - sm))

;; ==========
;; Bool Logic
;; ==========

;; TODO: consider put these funcs to utils.rkt

(define (bool-and v1 v2)
  (boolean->integer (and (integer->boolean v1)
                         (integer->boolean v2))))

(define (bool-or v1 v2)
  (boolean->integer (or (integer->boolean v1)
                        (integer->boolean v2))))

(define (OP_BOOLAND sm)
  (OP_BINARY_ARITH #:name 'OP_BOOLAND bool-and sm))

(define (OP_BOOLOR sm)
  (OP_BINARY_ARITH #:name 'OP_BOOLOR bool-or sm))

;; TODO: check other `equal?' functions
(define (OP_NUMEQUAL sm)
  (OP_BINARY_ARITH #:name 'OP_NUMEQUAL (lambda (top second)
                                         (boolean->integer (= top second))) sm))

(define (OP_NUMEQUALVERIFY sm)
  (OP_VERIFY (OP_NUMEQUAL sm)))

(define (OP_NUMNOTEQUAL sm)
  (OP_BINARY_ARITH #:name 'OP_NUMNOTEQUAL (lambda (top second)
                                            (boolean->integer (not (= top second)))) sm))

(define (OP_LESSTHAN sm)
  (OP_BINARY_ARITH #:name 'OP_LESSTHAN (lambda (top second)
                                         (boolean->integer (< top second))) sm))

(define (OP_GREATERTHAN sm)
  (OP_BINARY_ARITH #:name 'OP_GREATERTHAN (lambda (top second)
                                            (boolean->integer (> top second))) sm))

(define (OP_LESSTHANOREQUAL sm)
  (OP_BINARY_ARITH #:name 'OP_LESSTHANOREQUAL (lambda (top second)
                                                (boolean->integer (<= top second))) sm))

(define (OP_GREATERTHANOREQUAL sm)
  (OP_BINARY_ARITH #:name 'OP_GREATERTHANOREQUAL (lambda (top second)
                                                   (boolean->integer (>= top second))) sm))

(define (OP_MIN sm)
  (OP_BINARY_ARITH #:name 'OP_MIN min sm))

(define (OP_MAX sm)
  (OP_BINARY_ARITH #:name 'OP_MAX max sm))

(define (OP_WITHIN sm)
  (OP_TERNARY_ARITH #:name 'OP_WITHIN
                    (λ (upper lower x)
                      (boolean->integer (and (lower . <= . x) (x . < . upper)))) sm))
;; =============
;; Cryptographic
;; =============

(define (OP_RIPEMD160 sm)
  (OP_UNARY #:name 'OP_RIPEMD160
            ripemd160 sm))

(define (OP_SHA1 sm)
  (OP_UNARY #:name 'OP_SHA1
            sha1 sm))

(define (OP_SHA256 sm)
  (OP_UNARY #:name 'OP_SHA256
            sha256 sm))

(define (OP_HASH160 sm)
  (OP_UNARY #:name 'OP_HASH160
            hash160 sm))

(define (OP_HASH256 sm)
  (OP_UNARY #:name 'OP_HASH256
            hash256 sm))

;; =============
;; Reserved
;; =============

(define (OP_RESERVED sm)
  (struct-copy s-machine sm [tran-state #f]))

(define (OP_VER sm)
  (struct-copy s-machine sm [tran-state #f]))

(define (OP_VERIF sm)
  (struct-copy s-machine sm [tran-state #f]))

(define (OP_VERNOTIF sm)
  (struct-copy s-machine sm [tran-state #f]))

(define (OP_RESERVED1 sm)
  (struct-copy s-machine sm [tran-state #f]))

(define (OP_RESERVED2 sm)
  (struct-copy s-machine sm [tran-state #f]))

(define (OP_NOP1 sm)
  sm)

(define (OP_NOP4 sm)
  sm)

(define (OP_NOP5 sm)
  sm)

(define (OP_NOP6 sm)
  sm)

(define (OP_NOP7 sm)
  sm)

(define (OP_NOP8 sm)
  sm)

(define (OP_NOP9 sm)
  sm)

(define (OP_NOP10 sm)
  sm)



