#lang eopl

(define duple
  (lambda (n x)
    (cond
      ((zero? n) '())
      (else
       (cons x (duple (- n 1) x))))))

(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (car lst))
              (down (cdr lst))))))

(define swapper
  (lambda (s1 s2 slist)
  (cond
    ((null? slist) '())
    ((list? (car slist))
     (cons (swapper s1 s2 (car slist))
           (swapper s1 s2 (cdr slist))))
    (else
     (cond
       ((eqv? s1 (car slist)) (cons s2 (swapper s1 s2 (cdr slist))))
       ((eqv? s2 (car slist)) (cons s1 (swapper s1 s2 (cdr slist))))
       (else (cons (car slist) (swapper s1 s2 (cdr slist)))))))))

(define list-set
  (lambda (lst n x)
    (cond
      ((null? lst) '())
      ((eqv? n 0) (cons x (cdr lst)))
      (else (cons (car lst) (list-set (cdr lst) (- n 1) x))))))

(define count-occurrences
  (lambda (s lst)
    (cond
      ((null? lst) 0)
      ((list? (car lst)) (+
                          (count-occurrences s (car lst))
                          (count-occurrences s (cdr lst))))
      (else
       (cond
         ((eqv? s (car lst)) (+ 1 (count-occurrences s (cdr lst))))
         (else (count-occurrences s (cdr lst))))))))

(define propduct-filrer
  (lambda (s lst)
    (cond
      ((null? lst) '())
      (else
       (cons (list s (car lst)) (propduct-filrer s (cdr lst)))))))

(define product
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) '())
      (else
       (append (propduct-filrer (car ls1) ls2)
             (product (cdr ls1) ls2))))))

(define filter-in
  (lambda (pred lst)
    (cond
      ((null? lst) '())
      (else
       (if (pred (car lst))
           (cons (car lst) (filter-in pred (cdr lst)))
           (filter-in pred (cdr lst)))))))


(define list-index-rec
  (lambda (pred lst idx)
    (cond
      ((null? lst) #f)
      (else
       (if (pred (car lst))
           idx
           (list-index-rec pred (cdr lst) (+ 1 idx)))))))

(define list-index
  (lambda (pred lst)
    (list-index-rec pred lst 0)))

(define every?
  (lambda (pred lst)
    (cond
      ((null? lst) #t)
      (else
       (and (pred (car lst)) (every? pred (cdr lst)))))))

(define exists?
  (lambda (pred lst)
    (cond
      ((null? lst) #f)
      (else
       (or (pred (car lst)) (exists? pred (cdr lst)))))))


(define flatten
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((not (pair? lst)) (list lst))
      (else
       (append (flatten (car lst)) (flatten (cdr lst)))))))

(define merge
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) ls2)
      ((null? ls2) ls1)
      ((<= (car ls1) (car ls2)) (cons (car ls1) (merge (cdr ls1) ls2)))
      (else
       (cons (car ls2) (merge ls1 (cdr ls2)))))))

(define (take lst n)
  (cond
    ((or (zero? n) (null? lst)) '())  
    (else (cons (car lst) (take (cdr lst) (- n 1))))))

(define (drop lst n)
  (cond
    ((or
      (zero? n) (null? lst)) lst)  
    (else (drop (cdr lst) (- n 1))))) 

(define sort
  (lambda (lst)
    (if (or (null? lst) (null? (cdr lst)))
        lst
        (let* ((mid (quotient (length lst) 2))
               (left (take lst mid))
               (right (drop lst mid)))
          (merge (sort left) (sort right))))))

(define merge/pred
  (lambda (ls1 ls2 pred)
    (cond
      ((null? ls1) ls2)
      ((null? ls2) ls1)
      ((pred (car ls1) (car ls2)) (cons (car ls1) (merge/pred (cdr ls1) ls2 pred)))
      (else
       (cons (car ls2) (merge/pred ls1 (cdr ls2) pred))))))

(define sort/pred
  (lambda (pred lst)
    (if (or (null? lst) (null? (cdr lst)))
        lst
        (let* ((mid (quotient (length lst) 2))
               (left (take lst mid))
               (right (drop lst mid)))
          (merge/pred (sort/pred pred left) (sort/pred pred right) pred)))))

(define interior-node
  (lambda (content lson rson)
    (list content lson rson)))

(define leaf
  (lambda (content)
    content))

(define leaf?
  (lambda (bintree)
    (not (pair? bintree))))

(define lson cadr)
(define rson caddr)

(define contents-of
  (lambda (bintree)
    (if (leaf? bintree)
        bintree
        (car bintree))))




           

      
       
    