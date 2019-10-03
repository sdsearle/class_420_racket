;;================================================================================
;;==                         Lecture 15 Interpreter                             ==
;;================================================================================

;;================================================================================
;;==                               Top Level                                    ==
;;================================================================================

(define run
  (lambda (string)
    (eval-program (scan&parse string))))

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
    
    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    
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

(define eval-program
  (lambda (pgm)
    (cases program pgm
           (a-program (body) (eval-expression body (init-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
           (lit-exp     (datum)      datum)
           (var-exp     (id)         (apply-env env id))
           (primapp-exp (prim rands) (let ((args (eval-rands rands env)))
                                       (apply-primitive prim args)))
           (else                     (eopl:error 'eval-expression "Not here:~s" exp))
           )))

(define eval-rands                         ;; Evaluate all of the expressions in the list --rands--
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand                          ;; Evaluate an expression --rand--  ;; Just a wrapper for eval-expression
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitive                    ;; Apply a primitive procedure to a list of expressed values --args--
  (lambda (prim args)
    (cases primitive prim
           (add-prim       () (+ (car args) (cadr args)))
           (subtract-prim  () (- (car args) (cadr args)))
           (mult-prim      () (* (car args) (cadr args)))
           (incr-prim      () (+ (car args) 1))
           (decr-prim      () (- (car args) 1))
           )))

;;================================================================================
;;==                             Environments                                   ==
;;================================================================================

(define init-env                           ;; Parameterless function that creates an initial environment
  (lambda ()
    (extend-env '(i v x)
                '(1 5 10)
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
