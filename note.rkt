#lang racket

(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))


(define lat?
    (lambda (l)
        (cond
        ((null? l) #t)
        ((atom? (car l)) (lat? (cdr l)))
        (else #f))))

(define member?
    (lambda (a lat)
        (cond
        ((null? lat) #f)
        ((equal? a (car lat)) #t)
        (else (member? a (cdr lat))))))

(define rember
    (lambda (a lat)
        (cond
        ((null? lat) '())
        ((eq? a (car lat)) (cdr lat))
        (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
    (lambda (l)
        (cond
        ((null? l) '())
        (else (cons (car (car l)) (firsts (cdr l)))))))

(define seconds
    (lambda (l)
        (cond
        ((null? l) '())
        (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

(define insertR
    (lambda (new old lat)
        (cond
        ((null? lat) '())
        ((eq? old (car lat)) (cons old (cons new (cdr lat))))
        (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
    (lambda (new old lat)
        (cond
        ((null? lat) '())
        ((eq? old (car lat)) (cons new lat))
        (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
    (lambda (new old lat)
        (cond
        ((null? lat) '())
        ((eq? old (car lat)) (cons new (cdr lat)))
        (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
    (lambda (new o1 o2 lat)
        (cond
        ((null? lat) '())
        ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (subst2 new o1 o2 (cdr lat))))
        (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
    (lambda (a lat)
        (cond
        ((null? lat) '())
        ((eq? a (car lat)) (multirember a (cdr lat)))
        (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
    (lambda (new old lat)
        (cond
        ((null? lat) '())
        ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
        (else (cons (car lat) (multiinsertR new old (cdr lat)))))))


(define multiinsertL
    (lambda (new old lat)
        (cond
        ((null? lat) '())
        ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
        (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
    (lambda (new old lat)
        (cond
        ((null? lat) '())
        ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
        (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define o+
    (lambda (n m)
        (cond
        ((zero? m) n)
        (else (add1 (o+ n (sub1 m)))))))

(define o-
    (lambda (n m)
        (cond
        ((zero? m) n)
        (else (o- (sub1 n) (sub1 m))))))


(define addtup
    (lambda (tup)
        (cond
        ((null? tup) 0)
        (else (+ (car tup) (addtup (cdr tup)))))))

(define o*
    (lambda (n m)
        (cond
        ((zero? m) 0)
        (else (o+ n (o* n (sub1 m)))))))


(define tup+
    (lambda (tup1 tup2)
        (cond
        ((null? tup1) tup2)
        ((null? tup2) tup1)
        (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))


(define o>
    (lambda (n m)
        (cond
        ((zero? n) #f)
        ((zero? m) #t)
        (else (o> (sub1 n) (sub1 m))))))


(define o<
    (lambda (n m)
        (cond
        ((zero? m) #f)
        ((zero? n) #t)
        (else (o< (sub1 n) (sub1 m))))))

(define o=
    (lambda (n m)
        (cond
        ((o< n m) #f)
        ((o> n m) #f)
        (else #t))))

(define o^
    (lambda (n m)
        (cond
        ((zero? m) 1)
        (else (o* n (o^ n (sub1 m)))))))

(define o/
    (lambda (n m)
        (cond
        ((o< n m) 0)
        (else (add1 (o/ (o- n m) m))))))

(define length
    (lambda (l)
        (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))

(define pick
    (lambda (n lat)
        (cond
        ((zero? (o- n 1)) (car lat))
        (else (pick (sub1 n) (cdr lat))))))

(define rempick
    (lambda (n lat)
        (cond
        ((zero? (o- n 1)) (cdr lat))
        (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
    (lambda (l)
        (cond
        ((null? l) '())
        ((number? (car l)) (no-nums (cdr l)))
        (else (cons (car l) (no-nums (cdr l)))))))



(define all-nums
    (lambda (l)
        (cond
        ((null? l) '())
        ((number? (car l)) (cons (car l) (all-nums (cdr l))))
        (else (all-nums (cdr l))))))

(define eqan?
    (lambda (a1 a2)
        (cond
        ((and (number? a1) (number? a2)) (equal? a1 a2))
        ((or (number? a1) (number? a2)) #f)
        (else (eq? a1 a2)))))


(define occur
    (lambda (a lat)
        (cond
        ((null? lat) 0)
        ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
        (else (occur a (cdr lat))))))

(define one?
    (lambda (n)
        (eq? n 1)))

(define rember*
    (lambda (a l)
        (cond
        ((null? l) '())
        ((atom? (car l)) 
            (cond
                ((eq? a (car l)) (rember* a (cdr l)))
                (else (cons (car l) (rember* a (cdr l))))))
        (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
    (lambda (new old l)
        (cond
        ((null? l) '())
        ((atom? (car l))
            (cond
                ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
                (else (cons (car l) (insertR* new old (cdr l))))
            )
        )
        (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
    (lambda (a l)
        (cond
        ((null? l) 0)
        ((atom? (car l))
            (cond
                ((eq? a (car l)) (add1 (occur* a (cdr l))))
                (else (occur* a (cdr l)))
            )
        )
        (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(define leftmost
    (lambda (l)
        (cond
        ((atom? (car l)) (car l))
        (else (leftmost (car l))))))

(define eqlist?
    (lambda (l1 l2)
        (cond
            ((and (null? l1) (null? l2)) #t)
            ((or (null? l1) (null? l2)) #f)
            ((and (atom? (car l1)) (atom? (car l2)))
                (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
            ((or (atom? (car l1)) (atom? (car l2))) #f)
            (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
        )
    ))  



(define equal?
    (lambda (l1 l2)
        (cond
            ((and  (atom? l1) (atom? l2)) (eqan? l1 l2))
            ((or (atom? l1) (atom? l2)) #f)
            (else (eqlist? l1 l2)))))

(define numbered?
    (lambda (aexp)
        (cond
            ((atom? aexp) (number? aexp))
            (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(define value
    (lambda (nexp)
        (cond
            ((atom? nexp) nexp)
            (else
                (cond
                    ((eq? (car (cdr nexp)) '+) (o+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
                    ((eq? (car (cdr nexp)) '-) (o- (value (car nexp)) (value (car (cdr (cdr nexp))))))
                    ((eq? (car (cdr nexp)) '*) (o* (value (car nexp)) (value (car (cdr (cdr nexp))))))
                    ((eq? (car (cdr nexp)) '/) (o/ (value (car nexp)) (value (car (cdr (cdr nexp))))))
                )
            )
        )
    ))


(define 1st-sub-exp
    (lambda (nexp)
        (car (cdr nexp))))

(define 2nd-sub-exp
    (lambda (nexp)
        (car (cdr (cdr nexp)))))

(define operator
    (lambda (nexp)
        (car nexp)))

(define sero?
    (lambda (n)
        (null? n)))

(define edd1
    (lambda (n)
        (cons '() n)))

(define zub1
    (lambda (n)
        (cdr n)))

(define co+
    (lambda (n m)
        (cond
            ((sero? m) n)
            (else (co+ (edd1 n) (zub1 m))))))

(define set?
    (lambda (l)
        (cond
            ((null? l) #t)
            ((member? (car l) (cdr l)) #f)
            (else (set? (cdr l))))))

(define makeset
    (lambda (l)
        (cond
            ((null? l) '())
            ((member? (car l) (cdr l)) (makeset (cdr l)))
            (else (cons (car l) (makeset (cdr l)))))))

(define subset?
    (lambda (set1 set2)
        (cond
            ((null? set1) #t)
            ((member? (car set1) set2) (subset? (cdr set1) set2))
            (else #f))))

(define eqset?
    (lambda (set1 set2)
        (and (subset? set1 set2) (subset? set2 set1))))


(define intersect?
    (lambda (set1 set2)
        (cond
            ((null? set1) #f)
            ((member? (car set1) set2) #t)
            (else (intersect? (cdr set1) set2)))))

(define intersect
    (lambda (set1 set2)
        (cond
            ((null? set1) '())
            ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
            (else (intersect (cdr set1) set2)))))

(define union
    (lambda (set1 set2)
        (cond
            ((null? set1) set2)
            ((member? (car set1) set2) (union (cdr set1) set2))
            (else (cons (car set1) (union (cdr set1) set2))))))

(define intersectall
    (lambda (l)
        (cond
            ((null? l) '())
            ((null? (cdr l)) (car l))
            (else (intersect (car l) (intersectall (cdr l)))))))

(define a-pair?
    (lambda (x)
        (cond
            ((pair? x) #t)
            (else #f))))

(define first
    (lambda (p)
        (car p)))

(define second
    (lambda (p)
        (car (cdr p))))

(define third
    (lambda (p)
        (car (cdr (cdr p)))))

(define build
    (lambda (a b)
        (cons a (cons b '()))))

(define fun?
    (lambda (rel)
        (set? (firsts rel))))

(define revrel
    (lambda (rel)
        (cond
            ((null? rel) '())
            (else (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel)))))))

(define revpair
    (lambda (p)
        (build (second p) (first p))))

(define fullfun?
    (lambda (rel)
        (set? (seconds rel))))


(define rember-f
    (lambda (test? a l)
        (cond
            ((null? l) '())
            ((test? a (car l)) (cdr l))
            (else (cons (car l) (rember-f test? a (cdr l)))))))

(define insertR-f
    (lambda (test? new old l)
        (cond
            ((null? l) '())
            ((test? old (car l)) (cons old (cons new (cdr l))))
            (else (cons (car l) (insertR-f test? new old (cdr l)))))))

(define insertL-f
    (lambda (test? new old l)
        (cond
            ((null? l) '())
            ((test? old (car l)) (cons new l))
            (else (cons (car l) (insertL-f test? new old (cdr l)))))))

(define seqL
    (lambda (new old l)
        (cons new (cons old l))))

(define seqR
    (lambda (new old l)
        (cons old (cons new l))))

(define seqS
    (lambda (new old l)
        (cons new l)))


(define insert-g
    (lambda (seq)
        (lambda (new old l)
            (cond
                ((null? l) '())
                ((eq? old (car l)) (seq new old (cdr l)))
                (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

;(define insertL (insert-g seqL))
;(define insertR (insert-g seqR))
;(define subst ( insert-g seqS))

#|
(define insertL
    (insert-g
        (lambda (new old l)
            (cons new (cons old l)))))

(define insertR
    (insert-g
        (lambda (new old l)
            (cons old (cons new l)))))
|#

(define atom-to-function
    (lambda (a)
        (cond
            ((eq? a '+) o+)
            ((eq? a '-) o-)
            ((eq? a '*) o*)
            ((eq? a '/) o/)
        )))


(define multirember&co
    (lambda (a lat col)
        (cond
            ((null? lat) (col '() 0))
            ((eq? a (car lat)) (multirember&co a (cdr lat) (lambda (newlat found) (col newlat (add1 found)))))
            (else (multirember&co a (cdr lat) (lambda (newlat found) (col (cons (car lat) newlat) found)))))))

(define multiinsertLR&co
    (lambda (new old lat col)
        (cond
            ((null? lat) (col '() 0 0))
            ((eq? old (car lat)) (multiinsertLR&co new old (cdr lat) (lambda (newlat found changed) (col (cons old (cons new newlat)) (add1 found) (add1 changed)))))
            (else (multiinsertLR&co new old (cdr lat) (lambda (newlat found changed) (col (cons (car lat) newlat) found changed)))))))


(define even?
    (lambda (n)
        (= (remainder n 2) 0)))

(define evens-only*&co
    (lambda ( l col)
        (cond
            ((null? l)
                (col '() 1 0))
            ((atom? (car l))
                (cond
                    ((even? (car l))
                        (evens-only*&co (cdr l)
                            (lambda (newl p s)
                                (col (cons (car l) newl) (* (car l) p) s))))
                    (else (evens-only*&co (cdr l)
                            (lambda (newl p s)
                                (col newl p (+ (car l) s)))))))
            (else (evens-only*&co (car l)
                    (lambda (newl p s)
                        (evens-only*&co (cdr l)
                            (lambda (newl2 p2 s2)
                                (col (cons newl newl2) (* p p2) (+ s s2))))))))))


;if lat is full of numbers then looking can not stop,so the looking is called partial functions
(define looking
    (lambda (a lat)
        (keep-looking a (pick 1 lat) lat)))


(define keep-looking
    (lambda (a sorn lat)
        (cond
            ((number? sorn) (keep-looking a (pick sorn lat) lat))
            (else (eq? a sorn)))))

(define entity
    (lambda (x)
        (entity x)))

;if last-try returns true, then and (will-stop? last-try) (entity x) witll return false, it is conflicted
; if last-try returns false, then and (will-stop? last-try) (entity x) witll return false, it is conflicted
;so can not define will-stop? as below
#|
(define last-try
    (lambda (x)
        (and (will-stop? last-try) (entity x))))
|#