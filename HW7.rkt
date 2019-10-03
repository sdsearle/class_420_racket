#lang eopl

(define pow (lambda (n x)
              (if (zero? n)
                  1
                  (* x (pow (- n 1) x)))))
(define list-len (lambda (lon)
                   (if (null? lon)
                       0
                       (+ 1 (list-len(cdr lon)) ))))

#|
<lon>
(<number>. <lon>)
(-7 . (<number . <lon>))
(-7 . (3 . (<number . lon)))
(- 7 . (3 . (14. ())))
|#

(define list-of-numbers?
  (lambda (lst)
    (if (list? lst)
            (if (null? lst)
                #t
                (and (number? (car lst))
                     (list-of-numbers? (cdr lst))))
            #f)))