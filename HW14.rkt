#lang eopl

(define list-index (lambda (pred ls)
                     (cond ((null? ls) #f)
                           ((pred (car ls)) 0)
                           (else (let ((list-index-r (list-index pred (cdr ls))))
                                   (if (number? list-index-r)
                                       (+ list-index-r 1)
                                       #f))))))

(define list-find-position (lambda (sym los)
                             (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define-datatype enviroment enviroment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of racket-value?))
                       (env enviroment?)))

(define racket-value? (lambda (val)
                        #t))

(define empty-env2 (lambda () (empty-env-record)))

(define extend-env2 (lambda (syms vals env)
                     (extended-env-record syms vals env)))

(define apply-env2 (lambda (env sym)
                    (cases enviroment env
                      (empty-env-record () (eopl:error 'apply-env "No binding for ~s" sym))
                      (extended-env-record (syms vals env1)
                                           (let ((pos (list-find-position sym syms)))
                                             (if (number? pos)
                                                 (list-ref vals pos)
                                                 (apply-env2 env1 sym)))))))

(define empty-env (lambda () (lambda (sym) (eopl:error 'apply-env "No binding for ~s" sym))))

(define extend-env (lambda (syms vals env)
                     (lambda (sym)
                       (let ((pos (list-find-position sym syms)))
                          (if (number? pos)
                              (list-ref vals pos)
                              (apply-env env sym))))))

(define apply-env (lambda (env sym) (env sym)))

(define ev (extend-env '(z y c) '(100 200 300) (extend-env '(a b c) '(5 10 20) (empty-env))))


;A
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

(define is-lambda?
  (lambda (lst)
    (eq? (car lst) 'lambda)))

(define lambda-formals
  (lambda (lst) (cadr lst)))

(define lambda-body
  (lambda (lst) (caddr  lst)))

(define bound-aux '())

;1 parse the input - list with symbols
;2 If we find a declaration <-- remembered
;3 if bound or not

#|
(define bound-vars (lambda (exp) (bound-vars-aux exp '())))

(define bound-vars-aux (lambda (exp env)
                     (cond [(symbol? exp) (if (member exp env)
                                              (list exp)
                                              '())]
                           [(is-lambda? exp) (bound-vars-aux (lambda-body exp)
                                                             (append (lambda-formals exp) env))]
                           [else (union (bound-vars-aux (car exp) env) (bound-vars-aux (cadr exp) env)) ])))

(define free-vars (lambda (exp) (free-vars-aux exp '())))

(define free-vars-aux (lambda (exp env)
                     (cond [(symbol? exp) (if (member exp env)
                                              '()
                                              (list exp))]
                           [(is-lambda? exp) (free-vars-aux (lambda-body exp)
                                                             (append (lambda-formals exp) env))]
                           [else (union (free-vars-aux (car exp) env) (free-vars-aux (cadr exp) env)) ])))
|#

(define free-vars
  (lambda (exp)
    (free-vars-aux (parse-expression exp) '())))

(define free-vars-aux (lambda (exp env)
                     (cases expression exp
                       [var-exp (exp) (if (member exp env)
                                              '()
                                              (list exp))]
                       [lambda-exp (id body) (free-vars-aux body
                                                             (cons id env))]
                       [app-exp (rator rand) (union (free-vars-aux rator env) (free-vars-aux rand env)) ])))

(define bound-vars (lambda (exp)
    (bound-vars-aux (parse-expression exp) '())))

(define bound-vars-aux (lambda (exp env)
                     (cases expression exp
                       [var-exp (exp) (if (member exp env)
                                              (list exp)
                                               '())]
                       [lambda-exp (id body) (bound-vars-aux body
                                                             (cons id env))]
                       [app-exp (rator rand) (union (bound-vars-aux rator env) (bound-vars-aux rand env)) ])))


(define parse-expression (lambda (exp)
                    (cond [(symbol? exp) (var-exp exp)]
                          [(eqv? (car exp) 'lambda)
                           (lambda-exp (caadr exp) (parse-expression (caddr exp)))]
                          [else (app-exp (parse-expression (car exp)) (parse-expression (cadr exp))) ])))

(define-datatype expression expression?
  (var-exp (id symbol?))
  (lambda-exp (id symbol?) (body expression?))
  (app-exp (rator expression?) (rand expression?)))