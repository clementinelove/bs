#lang racket/base
;; structs.rkt
(require racket/contract
         racket/list
         bs/utils)

(module+ test
  (require rackunit))

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
    (push (list->stack n-removed-lst) elem)))

(module+ test
  (check-equal? (empty-stack) (list->stack '()))
  (check-equal? (push-dup-n (list->stack '(5 4 3 2 1)) 3) (list->stack '(5 4 3 5 4 3 2 1)))
  (check-equal? (push-tuck (list->stack '(1 2 3 4)))
                (list->stack '(1 2 1 3 4)))
  (check-equal? (roll (list->stack '(0 1 2 9)) 2)
                (list->stack '(2 0 1 9))))