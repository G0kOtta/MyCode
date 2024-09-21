#lang eopl

(define value?
  (lambda (value)
    #t))

(define-datatype env env?
  (empty-env-inter)
  (apply-env-inter
   (_var symbol?)
   (_env env?))
  (extend-env-inter
   (_var symbol?)
   (_val value?)
   (_env env?))
  (has-binding-inter
   (_var symbol?)
   (_env env?)))

(define empty-env
  (lambda ()
    (empty-env-inter)))

(define extend-env
  (lambda (var val env)
    (extend-env-inter var val env)))

(define apply-env
  (lambda (var E)
    (cases env E
      (empty-env-inter()
                      (eopl:error 'apply-env "Empty env"))
      (apply-env-inter (_var _env)
                       (eopl:error 'apply-env "error"))
      (has-binding-inter(_var _env)
                        (eopl:error 'apply-env "error"))

      (extend-env-inter(_var _val _env)
                       (if (eqv? _var var)
                           _val
                           (apply-env var _env))))))

(define has-binding?
  (lambda (var E)
    (cases env E
      (empty-env-inter() #f)
      (extend-env-inter(_var _val _env)
                       (if (eqv? _var var)
                           #t
                           (has-binding? var _env)))
      (has-binding-inter (_var _env)
                         (has-binding? var _env))
      (apply-env-inter (_var _env)
			    (has-binding? var _env)))))

(define-datatype stack stack?
  (empty-stack-record)
  (push-record
   (e value?)
   (s stack?)))

(define empty-stack
  (lambda ()
    (empty-stack-record)))

(define push
  (lambda (e s)
    (push-record e s)))

(define pop
  (lambda (st)
    (cases stack st
      (empty-stack-record ()
                          (eopl:error 'pop "Empty Stack"))
      (push-record (e s) s))))

(define top
  (lambda (st)
    (cases stack st
      (empty-stack-record ()
                          (eopl:error 'top "Empty Stack"))
      (push-record (e s) e))))

(define empty-stack?
  (lambda (st)
    (cases stack st
      (empty-stack-record () #t)
      (push-record (e s) #f))))

(define identifier?
  (lambda (value)
    (and (symbol? value)
         (not (eqv? value 'lambda)))))

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
      (leaf-node (num) `(leaf-node ,num))
      (interior-node (key left right)
                     (list
                      'interior-node
                      key
                      (bintree-to-list left)
                      (bintree-to-list right))))))


(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? 'lambda (car datum))
           (lambda-exp
            (caadr datum)
            (parse-expression (caddr datum)))
           (app-exp
            (parse-expression (car datum))
            (parse-expression (cadr datum)))))
      (else (eopl:error "exression not correct")))))

(define unparse-expression
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp (bound-var body)
                  (list 'lambda (list bound-var)
                        (unparse-expression body)))
      (app-exp (rator rand)
               (list (unparse-expression rator) (unparse-expression rand))))))
                  

      

    
  
      








                 