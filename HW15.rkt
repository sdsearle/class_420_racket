#lang eopl

(define for-each
  (lambda (proc lst)
    (if (null? lst)
        'done
        (begin (display (proc (car lst)))
               (newline)
               (for-each proc (cdr lst))))))

(define displayln (lambda lst
                    (for-each display lst)
                    (newline)))

;(define fact (lambda

(define a 10)

(define dec-a
  (lambda (x) (set! a (- a x))))

(define b (cons 5 10))

(define next-symbol
  (let ((c 0))
    (lambda ()
      (set! c (+ c 1))
      (string->symbol (string-append "g" (number->string c)))
      )))

#|
nextsymbol is equal to what let returns
(let ((c 0)) is only run once before the function is ever created.
aka nextsymbol is the function
|#