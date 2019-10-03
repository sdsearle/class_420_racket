#lang eopl
(define (++ x) (+ x 1))
(define (zero? x) (if
                    (= x 0) #t #f))
(define (zero?2 x) (cond
                    ((= x 0) #t)
                    ((< x 0) #f)
                    ((> x 0)#f)))

(define (cube x) (* x x x))
(define PI 3.14159265)
(define (new-sin x y) (cond
                        ((eq? y 'degrees) (sin (/ (* x PI) 180)))
                        ((eq? y 'radians) (sin x))
                        ("ERROR: Invalid type for new-sin")))