#lang racket/base
(require bs/opcodes bs/utils bs/structs bs/setup bs/display
         racket/string
         (for-syntax racket/base)
         (for-syntax syntax/parse))

(provide (all-from-out bs/opcodes))

(define-syntax-rule (bs-module-begin expr)
  (#%module-begin
   (module configure-runtime racket/base
     (require bs/setup)
     (do-setup!))
   (parameterize
       ([current-output-port (bs-output-port)])
     (void expr))))

(provide (rename-out [bs-module-begin #%module-begin])
         #%top-interaction
         #%app #%datum)

(define (report-invalid-transaction reason)
  (displayln (string-append "Invalid: " reason)))

(define (report-valid-transaction)
  (displayln  "OK: top stack item is a non-zero value"))

;; no op input should take a decimal of total 4 byte
(define FOUR-BYTE-INT-BOUND (/ (expt 256 4) 2))
;; for REPL support, use side effects to remember last sm state
(define SM (s-machine (empty-stack) (empty-stack) #t '()))

(define (handle-args . args)
  (for/fold ([sm SM]
             #:result
             (begin
               (set! SM sm) ; save s-machine state
               (display-s-machine sm)
               (displayln "")
               (let ([main-stk (s-machine-main-stk sm)]
                     [level (s-machine-level sm)]
                     [tran-state (s-machine-tran-state sm)])
                 (cond
                   [(not tran-state)
                    (report-invalid-transaction "this transaction was being marked as invalid")]
                   [(not (null? level))
                    ;(displayln level) ;for debug
                    (report-invalid-transaction "unbalanced OP_IF exist")]
                   [(stack-empty? main-stk)
                    (report-invalid-transaction "main stack is empty after execution")]
                   [else
                    (let ([top-item (top main-stk)])
                      (if (= (bytes->integer top-item #t #f) 0)
                          (report-invalid-transaction "top main stack item is 0 after execution")
                          (report-valid-transaction)))])))
             ;; TODO: just for test: show the current state of the stack
             )
            ([op (in-list args)])
    #:break (not (s-machine-tran-state sm))
    #;(displayln (s-machine-level sm)) ; DEBUG: show current level
    (if (or (null? (s-machine-level sm))
            (not (car (s-machine-level sm))))
        ;; when (s-machine-level sm) is empty, execute any command since it's not in an OP_IF block
        ;; when it's not empty, check (car (s-machine-level sm)): if it's #f then do execute
        (begin
          ;#;
          (with-handlers ([exn:fail?
                           (λ (e)
                             (let* ([exn-msg (exn-message e)]
                                    [sym-and-msg (string-split exn-msg #rx": ")]
                                    [sym (string->symbol (car sym-and-msg))]
                                    [msg (cadr sym-and-msg)])
                               (raise-syntax-error sym msg op)))])
            ((syntax-e op) sm))
          ;; use below exp for debugging
          #;
          ((syntax-e op) sm))
        ;; when level list is not emtpy and (top (s-machine-level sm)) => #t
        ;; which means skip current command until OP_ELSE or OP_ENDIF
        (let ([proc (syntax-e op)])
          (if (skipping-executable? proc)
              (proc sm)
              sm)))))


;; =======================
;;   Syntax Transformers
;; =======================

(provide bs-program
         pushdata-stat
         size)

;; Every stat/op will be put back into syntax objects with their source location,
;; for error report.
(define-syntax bs-program
  (λ (stx)
    (syntax-parse stx
      [(bs-program stat/op ...)
       #;
       (unless (identifier? #'name)
         (raise-syntax-error 'form
                             "expected an identifier for the form"
                             #'name))
       #'(handle-args (datum->syntax #f stat/op #'stat/op) ...)])))

;; size is either specified by hex-string or by exact-nonnegative-integer
(define-syntax-rule (size v)
  (if (hex-string? v)
      (string->number (string-append "#x" v))
      v))

;; A single byte can evaluate to 256 possible results
(define POSSIBLE-COMBINATIONS 256)

;; provide syntax check and error report
;; take care of checking with `size' and `data'
(define-syntax pushdata-stat 
  (syntax-rules ()
    [(pushdata-stat op size data)
     (λ (sm)
       (let ([old-main-stk (s-machine-main-stk sm)]
             [data-size-specifier-upperbound (sub1 (expt POSSIBLE-COMBINATIONS (special-op? 'op)))])
         (if (<= 0 size data-size-specifier-upperbound)
             (let* ([bytes-to-push (hex-string->bytes data)]
                    [actual-data-size (bytes-length bytes-to-push)])
               (if (= size actual-data-size)
                   (struct-copy s-machine sm [main-stk (push old-main-stk bytes-to-push)])
                   (report-pushdata-error size actual-data-size)))
             (report-size-specifier-error 'op data-size-specifier-upperbound))))]
    [(pushdata-stat size data)
     (λ (sm)
       (let ([old-main-stk (s-machine-main-stk sm)])
         (let* ([bytes-to-push (hex-string->bytes data)]
                [actual-data-size (bytes-length bytes-to-push)])
           (if (= size actual-data-size)
               (struct-copy s-machine sm [main-stk (push old-main-stk bytes-to-push)])
               (report-pushdata-error size actual-data-size)))))]))

(define (report-size-specifier-error op upperbound)
  (error op
         "data size specifier should specify size in between 0 and ~s bytes (both inclusive)"
         upperbound))

;; the error below usually do not signal errors but mark transaction state as #f
(define (report-pushdata-error size actual-size)
  (error 'OP_PUSHDATA
         "specified size (~s bytes) is not equal to the actual size of data being pushed (~s bytes)"
         size actual-size))
