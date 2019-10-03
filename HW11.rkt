#lang eopl

;binary tree
(define-datatype btree btree?
  (leaf-node (datum number?))
  (interior-node (key symbol?)
                 (left btree?)
                 (right btree?)))

(define-datatype slist slist?
  (empty-slist)
  (full-slist (first symbol-exp?)
              (rest slist?)))

(define-datatype symbol-exp symbol-exp?
  (symbol-se (data symbol?))
  (list-se (data slist?)))

(define atree (interior-node 'k (interior-node 'l (leaf-node 5) (leaf-node 7)) (leaf-node 6)))

(define leaf-sum (lambda (tree)
                   (cases btree tree
                     (leaf-node (datum) datum)
                     (interior-node (k l r) (+ (leaf-sum l) (leaf-sum r))))))

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

(define bound-vars2 (lambda (exp)
                         (cond [(symbol? exp) exp]
                           [(is-lambda? exp)
                                (let ((b (lambda-formals exp))) (let ((a (bound-vars (lambda-body exp)))) union a b)) ]
                           [else (list (bound-vars(car exp)) (bound-vars (cadr exp)))])))

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

;B
#|
(lambda (x.1)
               (lambda (y.1)
                       ((lambda (x.2)
                                (x.2 y.1))
                       x.1)))

(lambda (z.1)
               ((lambda (a.1 b.1 c.1)
                       (a.1 (lambda (a.2) (+ a.2 c.1)) b.1))
               (lambda (f.1 x.1)
                       (f.1 (z.1 x.1)))))
|#