#lang eopl

(define is-if? (lambda (lst) (eq? (car lst) 'if)))
(define if-test  (lambda (lst) (cadr lst)))
(define if-true  (lambda (lst) (caddr lst)))
(define if-else  (lambda (lst) (cadddr lst)))
(define is-lambda? (lambda (lst) (eq? (car lst) 'lambda)))
(define lambda-formals  (lambda (lst) (cadr lst)))
(define lambda-body  (lambda (lst) (caddr lst)))

(define lexical-address
   (lambda (exp)
     (lexical-address-aux exp '())))

(define application-exp2
  (lambda (exp env)
    (if (null? exp)
        exp
        (cons (lexical-address-aux (car exp) env)  (application-exp (cdr exp) env)))))

(define application-exp (lambda (lst env)
                          (map (lambda (exp) (lexical-address-aux exp env)) lst)))

(define lexical-address-aux
 (lambda (exp env)
   (cond ((null? exp) '()) ;at end of the expression
         ((symbol? exp) (cons exp (make-var-ref exp env))) ;variable reference
         ((is-if? exp) (list 'if ;if expression
                             (lexical-address-aux (if-test exp) env) 
                             (lexical-address-aux (if-true exp) env)
                             (lexical-address-aux (if-else exp) env)))
         ((is-lambda? exp)  (list 'lambda ;lambda expression
                                  (lambda-formals exp);keeps the formals to be printed
                                  (lexical-address-aux (lambda-body exp) (cons (lambda-formals exp) env))))
         (else (application-exp exp env)))));application expression

(define make-var-ref
  (lambda (id env)
    (lambda-formal-list id env 0)))

;( define make-var-ref (lambda (sym env) (list sym 'free)))

(define lambda-formal-list
  (lambda (id env num)
    (if (null? env)
         (list 'free)
         (if (member id (car env))
             ;is member
             (list ': num (lambda-formal-pos id (car env) 0))
             ;not member
             (lambda-formal-list id (cdr env) (+ 1 num))))))

(define lambda-formal-pos
  (lambda (id env num)
    (if (eq? id (car env))
             num
             (lambda-formal-pos id (cdr env) (+ num 1)))))