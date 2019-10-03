#lang eopl

(define list-of-numbers?
  (lambda (lst)
    (if (list? lst)
            (if (null? lst)
                #t
                (and (number? (car lst))
                     (list-of-numbers? (cdr lst))))
            #f)))

(define list-sum (lambda (lst)
                   (if (null? lst)
                       ;empty-- true
                       0
                       ;full-- false
                       (+ (car lst) (list-sum (cdr lst))))))

(define remove-first (lambda (s los)
                        (if (null? los)
                            ;empty
                            '()
                            ;full
                            (if(eq? s (car los))
                                 ;true
                                 (cdr los)
                                 ;false
                                 (cons (car los) (remove-first s (cdr los)))))))

(define remove-all (lambda (s los)
                        (if (null? los)
                            ;empty
                            '()
                            ;full
                            (if(eq? s (car los))
                                 ;true
                                 (remove-all s (cdr los))
                                 ;false
                                 (cons (car los) (remove-all s (cdr los)))))))

(define subst (lambda (new old slst)
                (if (null? slst)
                    ;empty
                    '()
                    ;full the first param in cons is a member inside a list
                    (cons (subst-se new old (car slst)) (subst new old (cdr slst))))))

(define subst-se (lambda (new old se)
                   (if (symbol? se)
                       ;symbol
                       (if (eq? old se) new se)
                       ;slst
                       (subst new old se))))

;1
(define duple (lambda (n x)
                (if (zero? n)
                    ;true
                    '()
                    ;false
                    (cons x (duple (- n 1) x)))))
;2
(define invert (lambda (lst)
                 (if (null? lst)
                     ;empty
                     '()
                     ;full
                     (cons(list(cadr(car lst)) (car(car lst))) (invert (cdr lst))))))

;3
(define filter-in (lambda (pred lst)
                    (if (null? lst)
                             ;true
                             '()
                             ;false
                             (if (pred (car lst))
                                 ;true
                                 (cons (car lst) (filter-in pred (cdr lst)))
                                 (filter-in pred (cdr lst))))))
                             
;4
(define every? (lambda (pred lst)
                 (if (null? lst)
                     ;empty
                     #t
                     ;full
                     (and (pred (car lst)) (every? pred (cdr lst))))))
;5
(define exists? (lambda (pred lst)
                 (if (null? lst)
                     ;empty
                     #f
                     ;full
                     (or (pred (car lst)) (exists? pred (cdr lst))))))