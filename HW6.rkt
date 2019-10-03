#lang eopl

(define square (lambda (x) (* x x)))

(define f2 (lambda (x y)
            (let ((a (+ 1 (* x y)))
                  (b (- 1  y)))
              (+ (* x (square a)) (* y b) (* a b)))))

(define x 5)

(define a 0)

(let ((a 3))
  (let ((b (+ a 1)))
    (+ a b)))

(let* ((a 3)
       (b (+ a 1)))
  (+ a b))

((lambda (a b)
   (+ a b))
   3 4)

(let ((sqr (lambda (x) (* x x)))
      (cube (lambda (x) (* x x x))))
  (+ (sqr 3) (cube 4)))

;(letrec ((f (lambda () (f))))
;  (f))

(letrec ((f (lambda (x) (if (= x 0)
                            1
                            (* x (f (- x 1)))))))
  (f 5))

(if (and (= 5 5) (= 4 4)) #t #f)

(define div (lambda (x) (if (and (> x 0) (not (= x 0)) (/ 5 0))
                            (/ x 2)
                            'dividebyzero)))

(define my-and (lambda args
                 (if (car args)
                     (apply my-and (cdr args))
                     #f)))

(define even-parity2 (lambda lst (if (= (length lst) 2)
                                    (if(= (modulo
                                       (+
                                        (if (= (car lst) 1)
                                            1
                                            0)
                                     (if (= (cadr lst) 1)
                                         1
                                         0))
                                       2)
                                         0)
                                       #t
                                       #f)
                                    (if(= (modulo
                                           (+
                                            (if (= (car lst) 1)
                                                1
                                                0)
                                            (if (= (cadr lst) 1)
                                                1
                                                0)
                                            (if (= (caddr lst) 1)
                                                1
                                                0))
                                           2)
                                          0)
                                       #t
                                       #f))))

;PART A

(define sum (lambda lst (if (= (length lst) 2)
                              (+ (car lst) (cadr lst))
                              (+ (+(car lst) (cadr lst)) (caddr lst)))))

(define even-parity (lambda lst (if (= (length lst) 2)
                  (if(= (modulo
                         (sum (car lst) (cadr lst))
                         2)
                        0)
                     #t
                     #f)
                  (if(= (modulo
                         (sum (car lst) (cadr lst) (caddr lst))
                         2)
                        0)
                     #t
                     #f))))

#|
((lambda (a b)
          (+ a b))
          3 (+ 2 3))

 

Java's syntactic sugar are the following:

for loops, lambda syntax, compound assignment operator, arrayList, etc.

For loops are like while loops.

Lambda syntax is basically just a more compact version of a method.

compound assignment operators are like var = var + var

an arrayList is similar to a linked list.
|#
