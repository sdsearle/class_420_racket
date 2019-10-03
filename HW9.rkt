#lang eopl

(define anno (lambda (lst) (anno-aux lst 1)))

(define anno-aux (lambda (lst n)
               (if (null? lst)
               ;empty
               '()
               ;full
               (cons (list (car lst) n) (anno-aux (cdr lst) (+ n 1))))))

(define flatten (lambda (slst)
                  (if (null? slst)
                      ;empty
                      '()
                      ;full
                      (append (flatten-se (car slst)) (flatten (cdr slst))))))

(define flatten-se (lambda (se)
                     (if (symbol? se)
                         ;symbol
                         (list se)
                         ;slst
                         (flatten se))))

;1
(define vector-index (lambda (pred v)
                       (if (number? (vector-index-ref pred v 0))
                                    (vector-index-ref pred v 0)
                                    #f)))
                                    

(define vector-index-ref (lambda (pred v n)
                           (if (= n (vector-length v))
                               #f
                               (if (pred (vector-ref v n))
                                   ;true
                                   n
                                   ;false
                                   (vector-index-ref pred v (+ n 1)))                               )))

;2
(define list-set (lambda (lst n x)
                   (if (null? lst)
                       '()
                       (if (zero? n)
                           ;zero
                           (cons x (list-set (cdr lst) (- n 1) x))
                           ;not zero
                           (cons (car lst) (list-set (cdr lst) (- n 1) x))))))

;3
(define product (lambda (los1 los2)
                  (if (null? los1)
                      ;empty
                      '()
                      ;full
                      (append (product2 (car los1) los2) (product (cdr los1) los2)))))

(define product2 (lambda (elem los2)
                   (if (null? los2)
                       ;empty
                       '()
                       ;full
                       (cons (list elem (car los2)) (product2 elem (cdr los2))))))

;4
(define down (lambda (lst)
               (if (null? lst)
                   ;empty
                   '()
                   ;full
                   (cons (list (car lst)) (down (cdr lst))))))

;5
(define swapper (lambda (s1 s2 slist)
                  (if (null? slist)
                      ;empty
                      '()
                      ;full
                      (cons(swapper-ex s1 s2 (car slist)) (swapper s1 s2 (cdr slist))))))

(define swapper-ex (lambda (s1 s2 se)
                     (if (symbol? se)
                         ;symbol
                         (cond [(eq? se s1) s2]
                               [(eq? se s2) s1]
                               [else se])
                         ;not
                         (swapper s1 s2 se))))