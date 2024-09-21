#lang eopl

(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (var val env)
    (cons (list var val) env)))

(define empty-env? null?)

(define extend-env-record->sym
  (lambda (env)
    (car (car env))))

(define extend-env-record->val
  (lambda (env)
    (cadr (car env))))

(define extend-env-record->old-env
  (lambda (env)
    (cdr env)))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors
                "Looking for a ~s, found ~s"
                variant
                value)))

(define value-of->vals
  (lambda (exps env)
    (cond
      ((null? exps) '())
      (else
       (cons (value-of (car exps) env)
             (value-of->vals
              (cdr exps) env))))))

(define extend-env->vals
  (lambda (vars vals env)
    (cond ((null? vars) env)
          (else
           (let ((var (car vars))
                 (val (car vals)))
           (extend-env->vals (cdr vars) (cdr vals)
                             (extend-env var val env)))))))

(define extend-env-iter
  (lambda (vars vals env)
    (cond
      ((null? vars) env)
      (else
       (let ((val (value-of (car vals) env)))
         (extend-env-iter (cdr vars) (cdr vals)
                          (extend-env (car vars) val env)))))))

(define extend-env->lisp->vals
  (lambda (vars vals env)
    (cond
      ((null? vars) env)
      (else
       (let ((var (car vars))
             (val (expval->car vals)))
         (extend-env->lisp->vals (cdr vars) (expval->cdr vals) (extend-env var val env)))))))
  
(define-datatype expval expval?
  (num-val
   (num number?))
  (emptylist-val)
  (pair-val
   (car expval?)
   (cdr expval?))
  (bool-val
   (bool boolean?)))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (expval-extractor-error 'num val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool val)))))

(define expval->null?
  (lambda (val)
    (cases expval val
      (emptylist-val() (bool-val #t))
      (else (bool-val #f)))))

(define expval->car
  (lambda (val)
    (cases expval val
      (pair-val(car cdr) car)
      (else (expval-extractor-error 'car val)))))

(define expval->cdr
  (lambda (val)
    (cases expval val
      (pair-val(car cdr) cdr)
      (else (expval-extractor-error 'cdr val)))))

(define list->val
  (lambda (ls)
    (if (null? ls)
        (emptylist-val)
        (pair-val (car ls)
                   (list->val (cdr ls))))))

(define apply-element
  (lambda (env)
    (lambda (elem)
      (value-of elem env))))

(define cond->val
  (lambda (conds acts env)
    (cond ((null? conds) (eopl:error 'cond->val "No conditions"))
          ((expval->bool (value-of (car conds) env))
           (value-of (car acts) env))
          (else
           (cond->val (cdr conds) (cdr acts) env)))))
           

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

(define apply-env
  (lambda (env var)
    (if (empty-env? env)
        (eopl:error 'apply-env "No binding variant")
        (let ((_var (extend-env-record->sym env))
              (_val (extend-env-record->val env))
              (_old-env (extend-env-record->old-env env)))
          (if (eqv? _var var)
              _val
              (apply-env _old-env var))))))
        
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (add-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (+ num1 num2)))))
      (mult-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (* num1 num2)))))
      (div-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (/ num1 num2)))))
      (minus-exp (exp1)
                (let ((val1 (value-of exp1 env)))
                  (let ((num1 (expval->num val1)))
                    (num-val
                     (- 0 num1)))))
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      (greater?-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (bool-val
                     (> num1 num2)))))
      (less?-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (bool-val
                     (< num1 num2)))))
      (equal?-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (bool-val
                     (= num1 num2)))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (emptylist-exp () (emptylist-val))
      (null?-exp (exp) (expval->null? (value-of exp env)))
      (cons-exp (exp1 exp2) (pair-val (value-of exp1 env) (value-of exp2 env)))
      (car-exp (exp) (expval->car (value-of exp env)))
      (cdr-exp (exp) (expval->cdr (value-of exp env)))
      (list-exp (exp) (list->val (map (apply-element env) exp)))
      (print-exp (exp)
                 (let ((val (value-of exp env)))
                   (display val)
                   (num-val 1)))
      (cond-exp (conds acts) (cond->val conds acts env))
      (let*-exp (vars vals body)
                (value-of body (extend-env-iter vars vals env)))
      (unpack-exp (vars vals body)
		       (let ((_vals (value-of vals env)))
			(value-of body (extend-env->lisp->vals vars _vals env))))
      (let-exp (vars exps body)
               (let ((vals (value-of->vals exps env)))
                 (value-of body
                           (extend-env->vals vars vals env)))))))

(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [unary-operator ((or "car" "cdr" "minus" "print")) string] ; Use a symbol output leads to error, don’t know why.
    [binary-operator ((or "-" "*" "/" "+" "cons")) string]
    [n-ary-operator ("list") string]
    [bool-unary-operator ((or "null?" "zero?")) string]
    [bool-binary-operator ((or "equal?" "greater?" "less?")) string]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

(define the-grammar
  '((program (expression) a-program)
    (expression (identifier) var-exp)
    (expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("*" "(" expression "," expression ")") mult-exp)
    (expression ("/" "(" expression "," expression ")") div-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
    (expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("list" "(" (separated-list expression ",") ")" ) list-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression ("print" "(" expression ")") print-exp)
    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
