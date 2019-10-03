#lang eopl
;;================================================================================
;;==                         Lecture 21 Interpreter                             ==
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
    
    (expression (number)                                                    lit-exp)
    (expression (identifier)                                                var-exp)
    (expression (primitive "(" (separated-list expression ",") ")")         primapp-exp)
    (expression ("if" expression "then" expression "else" expression)       if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)   let-exp)
    ;(expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression ("proc" "(" (separated-list formal ",") ")" expression)              proc-exp)
    (expression ("(" expression (arbno expression) ")")                     app-exp)
    ;(expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)
    ;             "in" expression)                                           letrec-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list formal ",") ")" "=" expression)
                          "in" expression)                                   letrec-exp)
    (expression ("set" identifier "=" expression)                           varassign-exp)
    
    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-test-prim)

    (formal (identifier) value-formal)
    (formal ("var" identifier) ref-formal)
    
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
;;==                          Internal Datatypes                                ==
;;================================================================================

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec (vector-of target?))))

(define-datatype target target?
  (direct-target   (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define expval?                           ;; Is is an expressed value?
  (lambda (x)
    (or (number? x) (procval? x))))

(define vector-of                         ;; Ignores argument (doesn't really make sure it's a vector of --pred--)
  (lambda (pred) vector?))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
                (a-ref (pos vec) (cases target (vector-ref vec pos)
                                        (direct-target   (expvval) #t)
                                        (indirect-target (ref)     #f)))))))

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
           (lit-exp       (datum)                       datum)
           (var-exp       (id)                          (apply-env env id))
           (primapp-exp   (prim rands)                  (let ((args (eval-primapp-exp-rands rands env)))
                                                          (apply-primitive prim args)))
           (if-exp        (test-exp true-exp false-exp) (if (true-value? (eval-expression test-exp env))
                                                            (eval-expression true-exp env)
                                                            (eval-expression false-exp env)))
           ;(proc-exp      (ids body)                    (closure ids body env))
           (proc-exp (formals body)                        (closure formals body env))
      (let-exp       (ids rands body)              (let ((args (eval-let-exp-rands rands env)))
                                                          (eval-expression body (extend-env ids args env))))
           (app-exp       (rator rands)                 (let ((proc (eval-expression rator env)))
                                                          (if (procval? proc)
                                                              (apply-procval proc rands env)
                                                              (eopl:error 'eval-expression "Attempt to apply non-procedure ~s" proc))))
           ;(letrec-exp    (proc-names idss bodies letrec-body)
            ;              (eval-expression letrec-body
             ;                              (extend-env-recursively proc-names idss bodies env)))
      (letrec-exp (proc-names formals bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively  proc-names formals bodies env)))
      (varassign-exp (id rhs-exp)                  (begin (setref! (apply-env-ref env id)
                                                                        (eval-expression rhs-exp env))
                                                               1))
           (else                                        (eopl:error 'eval-expression "Not here:~s" exp))
           )))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env)) rands)))

(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env))))

(define eval-rands
      (lambda (formals rands env)
        (map (lambda (form x) (eval-rand form x env)) formals rands)))

#|
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))
|#

(define eval-rand
  (lambda (form rand env)
    (cases formal form
      (value-formal (id) (direct-target (eval-expression rand env)))
      (ref-formal (id)    (cases expression rand
           (var-exp (id) (let ((ref (apply-env-ref env id)))
                           (cases target (primitive-deref ref)
                                  (direct-target   (expval) (indirect-target ref))
                                  (indirect-target (ref1)   (indirect-target ref1)))))
           (else         (direct-target (eval-expression rand env))))))))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
           (add-prim       () (+ (car args) (cadr args)))
           (subtract-prim  () (- (car args) (cadr args)))
           (mult-prim      () (* (car args) (cadr args)))
           (incr-prim      () (+ (car args) 1))
           (decr-prim      () (- (car args) 1))
           (zero-test-prim () (if (zero? (car args)) 1 0))
           )))

;;================================================================================
;;==                               Booleans                                     ==
;;================================================================================

(define true-value?
  (lambda (x)
    (not (zero? x))))

;;================================================================================
;;==                              Procedures                                    ==
;;================================================================================

(define-datatype procval procval?
  (closure (ids (list-of formal?))
           (body expression?)
           (env environment?)))

(define apply-procval
  (lambda (proc rands env)
    (cases procval proc
           (closure (formals body env1) (let ((args (eval-rands formals rands env))
                                             (ids (get-ids formals)))
                                         (eval-expression body (extend-env ids args env1)))))))

(define get-ids (lambda (formals)
                  (map (lambda (form) (cases formal form
                                        (value-formal (id) id)
                                        (ref-formal (id) id)))
                       formals)))

;;================================================================================
;;==                              References                                    ==
;;================================================================================

;; a reference is a pointer to a vector containing targets.
;; the order of fields is funny, but leads to easier debugging.

(define primitive-deref
  (lambda (ref)
    (cases reference ref
           (a-ref (pos vec) (vector-ref vec pos)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
           (a-ref (pos vec) (vector-set! vec pos val)))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
           (direct-target   (expval) expval)
           (indirect-target (ref1)   (cases target (primitive-deref ref1)
                                            (direct-target   (expval) expval)
                                            (indirect-target (ref2)   (eopl:error 'deref "Illegal reference: ~s" ref1)))))))

(define setref!
  (lambda (ref expval)
    (cases target (primitive-deref ref)
           (direct-target   (expval1) (primitive-setref! ref (direct-target expval)))
           (indirect-target (ref1) (primitive-setref! ref1 (direct-target expval))))))

;;================================================================================
;;==                             Environments                                   ==
;;================================================================================

(define init-env
  (lambda ()
    (extend-env '(i v x)
                (map direct-target '(1 5 10))
                (empty-env))))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals vector?)              ;; You must put targets here to avoid crashing in extend-env
                       (env environment?)))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (if ((list-of target?) vals)
        (extended-env-record syms (list->vector vals) env)
        (begin (eopl:printf "extend-env: bad values ~%")
               (eopl:pretty-print vals)
               (eopl:error 'extend-env "")))))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each (lambda (pos ids body)
                      (vector-set! vec pos (direct-target (closure ids body env))))   ;; Change for by-ref
                    (iota len)
                    idss
                    bodies)
          env)))))

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end)
          '()
          (cons next (loop (+ 1 next)))))))

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
           (empty-env-record    ()              (eopl:error 'apply-env-ref "No binding for ~s" sym))
           (extended-env-record (syms vals env) (let ((position (rib-find-position sym syms)))
                                                  (if (number? position)
                                                      (a-ref position vals)
                                                      (apply-env-ref env sym)))))))

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

(define difference
  (lambda (set1 set2)
    (cond ((null? set1)           '())
          ((memv (car set1) set2) (difference (cdr set1) set2))
          (else                   (cons (car set1) (difference (cdr set1) set2))))))

;;================================================================================
;;==                               Debugging                                    ==
;;================================================================================

#|
(define eval-rand
  (lambda (rand env)
    (eopl:printf "eval-rand: rand = ~s ~%" rand)
    (eopl:pretty-print (printable-env env))
    (let ((ans (cases expression rand
                      (var-exp (id) (let ((ref (apply-env-ref env id)))
                                      (cases target (primitive-deref ref)
                                             (direct-target   (expval) (indirect-target ref))
                                             (indirect-target (ref1)   (indirect-target ref1)))))
                      (else         (direct-target (eval-expression rand env))))))
      (eopl:printf "eval-rand succeeded ~%")
      ans)))
|#

(define printable-env
  (lambda (env)
    (cases environment env
           (empty-env-record    ()              '())
           (extended-env-record (syms vals env)
                                (cons (map (lambda (sym)
                                             (let ((pos (rib-find-position sym syms)))
                                               (if (number? pos)
                                                   (let ((t1 (vector-ref vals pos)))
                                                     (cases target t1
                                                            (direct-target   (expval) (list sym (list 'my-direct-target 
                                                                                                      (printable-expval expval))))
                                                            (indirect-target (ref1)   (list sym (list 'my-indirect-target 
                                                                                                      (printable-expval (deref ref1)))))))
                                                   (eopl:error 'printable))))
                                           syms)
                                      (printable-env env))))))

(define printable-expval
  (lambda (v)
    (if (procval? v)
        (cases procval v
               (closure (ids body env) (list 'my-closure ids body (printable-env env))))
        v)))
