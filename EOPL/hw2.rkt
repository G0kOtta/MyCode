#lang eopl

(define empty-stack
  (lambda ()
    '()))

(define empty-stack?
  (lambda (stack)
    (equal? '() stack)))

(define push
  (lambda (stack val)
    (cons val stack)))

(define pop
  (lambda (stack)
    (cdr stack)))

(define top
  (lambda (stack)
    (car stack)))

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env) (report-no-binding-found search-var))
      ((eqv? search-var (caar env)) (cdar env))
      (else
       (apply-env (cdr env) search-var)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for: " search-var)))

(define empty-env?
  (lambda (env)
    (if (null? env)
        #t
        #f)))

(define has-binding?
  (lambda (env var)
    (cond
      ((null? env) #f)
      (else
       (or (eqv? (caar env) var)
           (has-binding? (cdr env) var))))))

(define extend-env*
  (lambda (var-list val-list env)
    (cond
      ((null? var-list) env)
      (else
       (let ((var (car var-list))
             (val (car val-list)))
         (extend-env* (cdr var-list) (cdr val-list)
                      (extend-env var val env)))))))


(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp)))))))


(define var-exp
  (lambda (var)
    (cons 'var-exp var)))

(define lambda-exp
  (lambda (var lc-exp)
    (list 'lambda-exp (list var) lc-exp)))

(define app-exp
  (lambda (rator rand)
    (list 'app-exp rator rand)))

(define var-exp?
  (lambda (lc-exp)
    (eqv? (car lc-exp) 'var-exp)))

(define lambda-exp?
  (lambda (lc-exp)
    (eqv? (car lc-exp) 'lambda-exp)))

(define app-exp?
  (lambda (lc-exp)
    (eqv? (car lc-exp) 'app-exp)))

(define var-exp->var
  (lambda (lcexp)
    (cdr lcexp)))

(define lambda-exp->bound-var
  (lambda (lc-exp)
    (caadr lc-exp)))

(define lambda-exp->body
  (lambda (lc-exp)
    (caddr lc-exp)))

(define app-exp->rator
  (lambda (lc-exp)
    (cadr lc-exp)))

(define app-exp->rand
  (lambda (lc-exp)
    (caddr lc-exp)))


    



    
    

