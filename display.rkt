#lang racket/base
(require bs/structs
         bs/utils)

(define (display-table col-n rows #:header-row [header-row #f])
  (define (col-longest-content-length coverage col-i)
    (let ([col (map (lambda (lst) (list-ref lst col-i)) coverage)])
      (apply max (map (λ (s) (string-length s)) col))))
  (define cov (if (list? header-row) (cons header-row rows) rows))
  (define len-vec
    (let ([length-vec (make-vector col-n)])
      (for ([col-i col-n])
        (vector-set! length-vec col-i
                     (col-longest-content-length cov col-i)))
      length-vec))
  
  ;; add margin to right side of content to left align
  (define (add-margin-to-right content longest-content-len)
    (let* ([content-len (string-length content)]
           [diff (- longest-content-len content-len)])
      (string-append content (make-string diff #\space))))
  
  (define (display-row row)
    (for ([content (in-list row)]
          [col-i col-n])
      (printf "| ~a " (add-margin-to-right content (vector-ref len-vec col-i))))
    (displayln "|"))
  
  (define (draw-h-bar)
    (define h-bar
      (for/fold ([bar "+"])
                ([col-i col-n])
        (string-append bar (make-string (+ (vector-ref len-vec col-i) 2) #\-) "+")))
    (displayln h-bar))
  ;; display rows
  (when (list? header-row)
    (display-row header-row)
    (draw-h-bar))
  (for-each display-row rows))

; EXAMPLE: 
; (draw-table 2 '(("0x1112" "0x1212")) #:header-row '("Main Stack" "Alternative Stack"))

(define (columns->rows cols)
  (define col-n (length cols))
  (define row-n
    (apply max (map (λ (col) (length col)) cols)))
  (define col-vectors (list->vector (map list->vector cols)))
  (for/fold ([rows '()]
             #:result (reverse rows))
            ([row-i row-n])
    (cons (reverse (for/fold ([row '()])
                             ([col-i col-n])
                     (define col-vec-i (vector-ref col-vectors col-i))
                     (define cont
                       (if (> row-i (- (vector-length col-vec-i) 1))
                           ""
                           (vector-ref col-vec-i row-i)))
                     (cons cont row)))
          rows)))

(define (display-s-machine sm)
  (define main-stk-lst (stack->list (s-machine-main-stk sm)))
  (define alt-stk-lst (stack->list (s-machine-alt-stk sm)))
  (define (bytes->hex-format-str bs)
    (if (= (bytes-length bs) 0)
        "NULL"
        (string-append "0x" (bytes->hex-string bs))))
  (define main-stk-hex-lst (map bytes->hex-format-str main-stk-lst))
  (define alt-stk-hex-lst (map bytes->hex-format-str alt-stk-lst))
  (define-values (columns header)
    (if (null? alt-stk-hex-lst)
        (values (list main-stk-hex-lst) '("Main Stack"))
        (values (list main-stk-hex-lst alt-stk-hex-lst) '("Main Stack" "Alt Stack"))))
  (display-table (length header) (columns->rows columns)
                 #:header-row header))

(provide display-s-machine)

; EXAMPLE: 
;    (columns->rows '(("0x01" "0x02" "0x03") ("0x04" "0x05" "0x06")))
; => '(("0x01" "0x04") ("0x02" "0x05") ("0x03" "0x06"))
