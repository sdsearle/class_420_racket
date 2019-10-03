#lang eopl

#|
(lambda (y)
  ((lambda (x) x) y))
;x and y are bound

(lambda (z) (x))
;x is free and z is nothing b/c it is not a reference

(lambda (x y)
  ((lambda (a)
     ( (x: 1 0) ( (a: 0 0) (y: 1 1) )))
   (x: 0 1) ))
;all are bound
;(var: body varNum
|#

;(member symbol list) returns a truth val if the symbol is in the list use for union

(define union (lambda (lst1 lst2)
                (undup2 (append (undup lst1 lst2) lst2))))

(define undup (lambda (lst1 lst2)
                (if (null? lst1)
                    ;empty
                    '()
                    ;full
                    (if (member (car lst1) lst2)
                        ;is member
                        (undup (cdr lst1) lst2)
                        ;not member
                        (cons (car lst1) (undup (cdr lst1) lst2))))))

(define undup2 (lambda (lst)
                (if (null? lst)
                    ;empty
                    '()
                    ;full
                    (if (member (car lst) (cdr lst))
                        ;is member
                        (undup2 (cdr lst))
                        ;not member
                        (cons (car lst) (undup2 (cdr lst)))))))

(define prod (lambda (lst)
               (if (null? lst)
                    ;empty
                    1
                    ;full
                    (* (car lst) (prod (cdr lst))))))

(define fold (lambda (pred base lst)
               (if (null? lst)
                    ;empty
                    base
                    ;full
                    (pred (car lst) (fold pred base(cdr lst))))))
