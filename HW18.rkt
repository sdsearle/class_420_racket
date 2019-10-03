#lang eopl

(define run (lambda (string) (eval-program (scan&parse string))))

;;================================================================================
;;==                       Grammatical Specification                            ==
;;================================================================================

(define the-lexical-spec
  '((whitespace (whitespace)                                   skip)
    (comment    ("%" (arbno (not #\newline)))                  skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number     (digit (arbno digit))                          number)))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number)                                            lit-exp)
    (expression (identifier)                                        var-exp)
    (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("proc" "(" (separated-list identifier "," ) ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") app-exp)
    
    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("minus") minus-prim)
    (primitive ("list") list-prim)
    (primitive ("cons") cons-prim)
    (primitive ("car") car-prim)
    (primitive ("cdr") cdr-prim)
    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

(define read-eval-print
  (sllgen:make-rep-loop "--> "
                        (lambda (pgm) (eval-program pgm))
                        (sllgen:make-stream-parser the-lexical-spec the-grammar)))

;;================================================================================
;;==                            The Interpreter                                 ==
;;================================================================================

(define eval-program (lambda (pgm)
                       (cases program pgm
                         (a-program (body) (eval-expression body (init-env))))))

(define eval-expression (lambda (exp env)
                          (cases expression exp
                            (lit-exp (datum) datum)
                            (var-exp (id) (apply-env env id))
                            (primapp-exp (prim rands) (let ((args (eval-rands rands env)))
                                                        (apply-primative prim args)))
                            (if-exp (test-exp true-exp else-exp) (if (true-value? (eval-expression test-exp env))
                                                                     (eval-expression true-exp env)
                                                                     (eval-expression else-exp env)))
                            (let-exp (ids rands body) (let ((args (eval-rands rands env)))
                                                        (eval-expression body (extend-env ids args env))))
                            (proc-exp (ids body) (closure ids body env))
                            (else (eopl:error 'eval-expression "Not Here: ~s" exp))                            
                            )))

(define eval-rands (lambda (rands env)
                     (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand (lambda (rand env)
                    (eval-expression rand env)))

(define apply-primative (lambda (prim args)
                          (cases primitive prim
                            (add-prim () (+ (car args) (cadr args)))
                            (subtract-prim () (- (car args) (cadr args)))
                            (mult-prim () (* (car args) (cadr args)))
                            (incr-prim () (+ 1 (car args)))
                            (decr-prim () (- (car args) 1))
                            (minus-prim () (- 0 (car args)))
                            (list-prim () args)
                            (cons-prim () (cons (car args) (cadr args)))
                            (car-prim () (caar args))
                            (cdr-prim () (cdar args))
                            )))

(define true-value? (lambda (x)
  (not (zero? x))))

(define-datatype procval procval?
  (closure (ids (list-of symbol?))
           (body expression?)
           (env environment?)))

;;================================================================================
;;==                             Environments                                   ==
;;================================================================================

(define init-env                           ;; Parameterless function that creates an initial environment
  (lambda ()
    (extend-env '(i v x emptylist)
                '(1 5 10 ())
                (empty-env))))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals vector?)              ;; You can put any type of expressed value in here
                       (env environment?)))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env                         ;; Add variables to an environment
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define apply-env                          ;; Looks up a variable in an environment
  (lambda (env sym)
    (cases environment env
           (empty-env-record    ()              (eopl:error 'apply-env "No binding for ~s" sym))
           (extended-env-record (syms vals env) (let ((position (rib-find-position sym syms)))
                                                  (if (number? position)
                                                      (vector-ref vals position)
                                                      (apply-env env sym)))))))

;; apply-env helper functions
(define rib-find-position
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond ((null? ls)      #f)
          ((pred (car ls)) 0)
          (else            (let ((list-index-r (list-index pred (cdr ls))))
                             (if (number? list-index-r)
                                 (+ list-index-r 1)
                                 #f))))))
