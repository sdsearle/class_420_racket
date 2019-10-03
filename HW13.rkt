#lang eopl

#|
(define-datatype expression expression?
  (lit-exp (dataum number?))
  (var-exp (id symbol?))
  (lambda-exp (id symbol?) (body expression?))
  (app-exp (rator expression?) (rand expression?)))

(define parse-exp (lambda (exp)
                    (cond [(number? exp) (lit-exp exp)]
                          [(symbol? exp) (var-exp exp)]
                          [(eqv? (car exp) 'lambda)
                           (lambda-exp (caadr exp) (parse-exp (caddr exp)))]
                          [else (app-exp (parse-exp (car exp)) (parse-exp (cadr exp))) ])))

(define unparse-exp (lambda (exp)
                      (cases expression exp
                        (lit-exp (datum) datum)
                        (var-exp (id) id)
                        (lambda-exp (id body) (list 'lambda (list id) (unparse-exp body)))
                        (app-exp (rator rand) (list (unparse-exp rator) (unparse-exp rand))))))
|#

;A
(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?))
  (empty-tree
   (null null?))
  )

(define leaf-sum
  (lambda (tree)
    (cases bintree tree
      (leaf-node (datum) datum)
      (interior-node (key left right)
                     (+ (leaf-sum left) (leaf-sum right)))
      (empty-tree
       (null) 0))
      ))

(define tree '(a (b (5 6)) (c (1 2))))
(define tree2 (empty-tree '()))

;B
(define-datatype expression expression?
                 (var-exp (id symbol?))
                 (lambda-exp (id symbol?)(body expression?))
                 (app-exp (rator expression?)(rand expression?)))

(define my-tree (app-exp (lambda-exp 'a (app-exp (var-exp 'a) (var-exp 'b))) (var-exp 'c)))