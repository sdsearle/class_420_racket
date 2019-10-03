#lang eopl

(define square (lambda (x) (* x x)))
(define sum-of-squares (lambda (x y)
                         (+ (square x) (square y))))

(define compose (lambda (f g)
                  (lambda (x)
                    (f (g x)))))

(define add2 (lambda (x) (+ x 2)))

(define add4 (compose add2 add2))

(define tmp (compose car cdr))

(define add (lambda (x y) (+ x y)))

(define curried-add (lambda (x)
                      (lambda (y) (+ x y))))

(define vadd (lambda lst lst))

(define vadd2 (lambda lst (if (= (length lst) 2)
                              (+ (car lst) (cadr lst))
                              (+ (car lst) (cadr lst) (caddr lst)))))

(define f
  (lambda (x)
     (lambda (y)
       (y x))))

(define g (f (list 1 2 3 4 5)))