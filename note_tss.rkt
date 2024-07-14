#lang racket

(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      [(empty? lat) #f]
      [else (or (is-first? (car lat) (cdr lat)))
                (two-in-a-row? (cdr lat))])))

(define is-first?
    (lambda (a lat)
        (cond
            ((null? lat) #f)
            (else (eq? a (first lat))))))

(define two-in-a-row-b?
    (lambda (preceding lat)
        (cond
            ((null? lat) #f)
            (else (or (eq? (car lat) preceding)
                      (two-in-a-row-b? (car lat) (cdr lat)))))))

(define two-in-a-row-r
    (lambda (lat)
        (cond
            ((null? lat) #f)
            (else (two-in-a-row-b? (car lat) (cdr lat))))))

(define sum-of-prefixes-b
    (lambda (sum tup)
        (cond
            ((null? tup) '())
            (else
                (cons (+ sum (car tup))
                      (sum-of-prefixes-b (+ sum (car tup)) (cdr tup)))))))

(define sum-of-prefixes
    (lambda (tup)
        (sum-of-prefixes-b 0 tup)))

(define one?
    (lambda (n)
        (eq? n 1)))

(define pick
    (lambda (n lat)
        (cond
            ((one? n) (car lat))
            (else (pick (- n 1) (cdr lat))))))

(define scramble-b
    (lambda (tup rev-pre)
        (cond
            ((null? tup) '())
            (else
                (cons (pick (car tup)
                        (cons (car tup) rev-pre))
                        (scramble-b (cdr tup)
                                    (cons (car tup) rev-pre)))))))

(define scramble
    (lambda (tup)
        (scramble-b tup '())))

(define Y
    (lambda (f)
        ((lambda (x) (f (lambda (y) ((x x) y))))
         (lambda (x) (f (lambda (y) ((x x) y)))))))

; (define multirember
;     (lambda (a lat)
;         ((Y (lambda (mr)
;                 (lambda (lat)
;                     (cond
;                         ((null? lat) '())
;                         ((eq? a (car lat)) (mr (cdr lat)))
;                         (else (cons (car lat) (mr (cdr lat))))))))
;             lat)))


(define multirember
    (lambda (a lat)
        (letrec ((mr (lambda (lat)
                        (cond
                            ((null? lat) '())
                            ((eq? a (car lat)) (mr (cdr lat)))
                            (else (cons (car lat) (mr (cdr lat))))))))
            (mr lat))))

(define multirember-f
    (lambda (test?)
        (letrec 
            ((m-f
                (lambda (a lat)
                    (cond
                        ((null? lat) '())
                        ((test? a (car lat)) (m-f a (cdr lat)))
                        (else (cons (car lat) (m-f a (cdr lat))))))))
            m-f)))

(define member?
    (lambda (a lat)
        (cond
            ((null? lat) #f)
            ((eq? a (car lat)) #t)
            (else (member? a (cdr lat))))))
            
(define intersect
    (lambda (set1 set2)
        (letrec
            ((A (lambda (set1)
                    (cond
                        ((null? set1) '())
                        ((member? (car set1) set2) (cons (car set1) (A (cdr set1))))
                        (else (A (cdr set1)))))))
            (cond
                ((null? set1) '())
                (else (A set1))))))

(define intersectall
    (lambda (lset)
        (call/cc
            (lambda (hop)
                (letrec 
                    ((A (lambda (lset)
                            (cond
                                ((null? (car lset)) (hop '()))
                                ((null? (cdr lset)) (car lset))
                                (else (I (car lset) (A (cdr lset)))))))
                     (I (lambda (s1 s2)
                            (letrec
                                ((J (lambda (s1)
                                    (cond
                                        ((null? s1) '())
                                        ((member? (car s1) s2)
                                         (J (cdr s1)))
                                         (else (cons (car s1) (J (cdr s1))))))))
                                (cond
                                    ((null? s2) (hop '()))
                                    (else (J s1)))))))
                    (cond
                        ((null? lset) '())
                        (else (A lset))))))))

(define rember-beyond-first
    (lambda (a lat)
        (letrec
            ((R (lambda (lat)
                    (cond
                        ((null? lat) '())
                        ((eq? (car lat ) a) '())
                        (else (cons (car lat) (R (cdr lat))))))))
            (R lat))))

(define rember-upto-last
    (lambda (a lat)
        (call/cc
            (lambda (skip)
                (letrec
                    ((R (lambda (lat)
                            (cond
                                ((null? lat) '())
                                ((eq? (car lat) a) (skip (R (cdr lat))))
                                (else (cons (car lat) (R (cdr lat))))))))
                    (R lat))))))

(define leftmost
    (lambda (l)
        (cond
            ((null? l) '())
            ((atom? (car l)) (car l))
            (else
                (let ((a (leftmost (car l))))
                    (cond
                        ((atom? a) a)
                        (else (leftmost (cdr l)))))))))

(define depth*
    (lambda (l)
        (cond
            ((null? l) 1)
            ((atom? (car l)) (depth* (cdr l)))
            (else
                (cond
                    ((> (depth* (cdr l)) (add1 (depth* (car l)))) (depth* (cdr l)))
                    (else (add1 (depth* (car l)))))))))                  

;(depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter)) 4

(define leftmost&co
    (lambda (l)
        (call/cc
            (lambda (skip)
                (lm l skip)))))

(define lm
    (lambda (l out)
        (cond
            ((null? l) '())
            ((atom? (car l)) (out (car l)))
            (else
                (begin
                    (lm (car l) out)
                    (lm (cdr l) out))))))


(define rm
    (lambda (a l oh)
        (cond
            ((null? l) (oh 'no))
            ((atom? (car l))
             (if (eq? (car l) a)
                (cdr l)
                (cons (car l) (rm a (cdr l) oh))))
            (else
                (if(atom?
                    (call/cc
                        (lambda (oh)
                            (rm a (car l) oh))))
                    (cons (car l) (rm a (cdr l) oh))
                    (cons (rm a (car l) 0) (cdr l)))))))
(define rember1*
    (lambda (a l)
        (if (atom? (call/cc
                    (lambda (oh)
                        (rm a l oh))))
            l
            (rm a l '()))))
; (define rember1*
;     (lambda (a l)
;         (try oh (rm a l oh) l)))

#|
(try x a b) = (call/cc (lambda (success)
                (call/cc
                    (lambda (x)
                        (success a)))
                b))
|#

(define try
    (lambda (x a b)
        (call/cc
            (lambda (success)
                (call/cc
                    (lambda (x)
                        (success a)))
                b))))

(define diner
    (lambda (food)
        (cons 'milkshake
            (cons food '()))))

(define x 
    (cons 'chicago
        (cons 'pizza
            '())))

(define dinerR
    (lambda (food)
        (set! x food)
        (cons 'milkshake
            (cons food '()))))

(define omnivore
    (let ((x 'minestrone))
        (lambda (food)
            (set! x food)
            (cons food
                (cons x '())))))
(define deep
    (lambda (m)
        (cond
            ((zero? m) 'pizsa)
            (else (cons (deepM (sub1 m)) '())))))

(define Rs '())
(define Ns '())

(define deepR
    (lambda (n)
        (set! Rs (cons (deep n) Rs))
        (set! Ns (cons n Ns))
        (deep n)))

(define find
    (lambda (n ns rs)
        (letrec
            ((A (lambda (ns rs)
                    (cond
                        ((null? ns) #f)
                        ((= (car ns) n) (car rs))
                        (else (A (cdr ns) (cdr rs)))))))
            (A ns rs))))

; (define deepM
;     (lambda (n)
;         (if (member? n Ns)
;             (find n Ns Rs)
;             (deepR n))))

; (define length
;     (let ((h (lambda (l) )))
;         (set! h
;             (lambda (l)
;                 (cond
;                     ((null? l) 0)
;                     (else (add1 (h (cdr l)))))))
;       h))



; (define length
;     (let ((h (lambda (l) 0)))
;         (set! h
;             (L (lambda (arg) (h arg))))
;         h))

(define Y!
   (lambda (f)
        (letrec
            ((h (f (lambda (arg) (h arg)))))
        h)))

(define L
    (lambda (length)
        (lambda (l)
            (cond
                ((null? l ) 0)
                (else (add1 (length (cdr l))))))))

(define deepM
    (let ((Rs '())
          (Ns '())
          (D (lambda (m)
                    (if (zero? m)
                        'pizza
                        (cons (deepM (sub1 m)) '())))))
            (lambda (n)
                (let ((exists (find n Ns Rs)))
                    (if (atom? exists)
                        (let ((deep (D n)))
                            (set! Rs (cons deep Rs))
                            (set! Ns (cons n Ns))
                            deep)
                            exists)))))

(define counter 0)
(define set-counter 0)
(define consC
    (let ((N 0))
        (set! counter
            (lambda ()
                N))
        (set! set-counter (lambda (n) (set! N n)))
        (lambda (x y)
            (set! N (add1 N))
            (cons x y))))

(define supercounter
    (lambda (f)
        (letrec
            ((S (lambda (n)
                (if (zero? n)
                    (f n)
                    (let ()
                        (f n)
                        (S (sub1 n)))))))
            (S 1000)
            (counter))))

(define lots
    (lambda (m)
        (cond
            ((zero? m) '())
            (else (cons (lots (sub1 m)) '())))))

(define lenkth
    (lambda (l)
        (cond
            ((null? l) 0)
            (else (add1 (lenkth (cdr l)))))))

(define kons
    (lambda (kar kdr)
        (lambda (selector)
            (selector kar kdr))))

(define kar
    (lambda (c)
        (c (lambda (a d) a))))

(define kdr
    (lambda (c)
        (c (lambda (a d) d))))

; (define my-pair (kons 1 2))

; my-pair => (lambda (selector) (selector 1 2))

; (kar my-pair) => (my-pair (lambda (a d) a)) 
; =>(lambda (selector) (selector 1 2)) (lambda (a d) a) => 1

(define bons
    (lambda (kar)
        (let ((kdr '()))
            (lambda (selector)
                (selector 
                    (lambda (x) (set! kdr x))
                    kar
                    kdr)))))
(define kar-bons
    (lambda (b)
        (b (lambda (k a d) a))))
(define kdr-bons
    (lambda (b)
        (b (lambda (k a d) d))))

; (define my-bons (bons 1))
; => (lambda (selector) (selector (lambda (x) (set! kdr x)) 1 kdr)
; (kar-bons my-bons)

(define set-kdr
    (lambda (c x)
        ((c (lambda (s a d) s)) x)))

(define kbons
    (lambda (a d)
        (let ((c (bons a)))
            (set-kdr c d)
            c)))

(define same?
    (lambda (c1 c2)
        (let ((t1 (kdr c1))
              (t2 (kdr c2)))
            (set-kdr c1 1)
            (set-kdr c2 2)
            (let ((v (= (kdr c1) (kdr c2))))
                (set-kdr c1 t1)
                (set-kdr c2 t2)
                v))))
                
(define toppings 0)

(define deepB
    (lambda (m)
        (cond
            ((zero? m) 
             (call/cc
                (lambda (jump)
                    (set! toppings jump)
                    'pizza)))
            (else (cons (deepB (sub1 m)) '())))))

(define deep&co
    (lambda (m k)
        (cond
            ((zero? m) (k 'pizza))
            (else
                (deep&co (sub1 m)
                    (lambda (x)
                        (k (cons x '()))))))))
(define t 0)
(define deep&coB
  (lambda (m k)
    (cond
      ((zero? m)
       (let ()
         (set! t k)
         (k 'pizza)))
      (else
       (deep&coB (sub1 m)
                 (lambda (x)
                   (k (cons x '()))))))))

(define leave 0)
(define walk
    (lambda (l)
        (cond 
            ((null? l) '())
            ((atom? (car l)) (leave (car l)))
            (else
                (let()
                    (walk (car l))
                    (walk (cdr l)))))))

(define start-it
    (lambda (l)
        (call/cc
            (lambda (here)
                (set! leave here)
                (walk l)))))

(define fill 0)
(define waddle
    (lambda (l)
        (cond
            ((null? l) '())
            ((atom? (car l))
                (let ()
                    (call/cc
                        (lambda (reset)
                            (set! fill reset)
                            (leave (car l))))
                            (waddle (cdr l))))
            (else
                (let ()
                    (waddle (car l))
                    (waddle (cdr l)))))))

(define start-it2
    (lambda (l)
        (call/cc
            (lambda (here)
                (set! leave here)
                (waddle l)))))