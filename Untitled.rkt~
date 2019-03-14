#lang racket
 
;; We create a structure that supports the
;; prop:exn:srcloc protocol.  It carries
;; with it the location of the syntax that
;; is guilty.
(define-struct (exn:fail:he-who-shall-not-be-named
                exn:fail)
  (a-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(struct exn:fail:he-who-shall-not-be-named
         (msg marks a-srcloc))
       (list a-srcloc)])))
 
;; We can play with this by creating a form that
;; looks at identifiers, and only flags specific ones.
(define-syntax (skeeterize stx)
  (syntax-case stx ()
    [(_ expr)
     (cond
       [(and (identifier? #'expr)
             (eq? (syntax-e #'expr) 'voldemort))
        (quasisyntax/loc stx
          (raise (make-exn:fail:he-who-shall-not-be-named
                  "oh dear don't say his name"
                  ;(current-continuation-marks)
                  (srcloc '#,(syntax-source #'expr)
                          '#,(syntax-line #'expr)
                          '#,(syntax-column #'expr)
                          '#,(syntax-position #'expr)
                          '#,(syntax-span #'expr)))))]
       [else
        ;; Otherwise, leave the expression alone.
        #'expr])]))
 
(define (f x)
  (* (skeeterize x) x))
 
(define (g voldemort)
  (* (skeeterize voldemort) voldemort))
 
;; Examples:
(f 7)
(g 7)
;; The error should highlight the use
;; of the one-who-shall-not-be-named
;; in g.