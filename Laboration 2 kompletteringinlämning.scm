(load "music-db.scm")
(load "quicksort-skel.scm")
(require (lib "trace.ss"))

;Problem 1

(define (atom? n)
  (if
   (or (null? n) (pair? n) )
       #f
       #t))

;Problem 2

(define (count-list lst)
  (if (null? lst)
      0
      (+ 1 (count-list(cdr lst)))))

;Problem 3

(define (keep-if pred lat)
  (cond
    ((null? lat) '())
    ((pred (car lat))
      (cons (car lat) (keep-if pred (cdr lat))))
     (else (keep-if pred (cdr lat)))))

;Problem 4

(define (first-n numb-of-ele lst)
  (cond
    ((= numb-of-ele 0)'())
    ((null? lst) '())
    (else (cons (car lst) (first-n (- numb-of-ele 1) (cdr lst))))))

;Problem 5

(define (range from to step)
  (if (> from to)
      '()
      (cons from (range (+ from step) to step))))

;Problem 6

;Iterative

(define (reverse-order lst)
  (define (reverse-iter lst res)
    (if (null? lst)
        res
        (reverse-iter (cdr lst) (cons (car lst) res))))
  (reverse-iter lst '()))

;Linear Recursive

(define (reverse-order-2 lst)
  (if (null? lst)
      lst
     (my-append (reverse-order-2 (cdr lst)) (cons (car lst) ()))))

(define (my-append lst1 lst2)
    (if (null? lst1)
        lst2
        (cons (car lst1) (my-append (cdr lst1) lst2))))

;Problem 7

(define (map-to-each funktion lst)
  (if (null? lst)
      '()
      (cons (funktion (car lst)) (map-to-each funktion (cdr lst)))))

;Problem 8

(define (count-all lst)
  (cond
    ((null? lst) 0)
    ((list? (car lst)) (+ (count-all (car lst)) (count-all (cdr lst))))
      (else (+ 1 (count-all(cdr lst))))))

(define (keep-if-all pred lst)
  (cond
    ((null? lst) '())
    ((list? (car lst)) (cons (keep-if-all pred (car lst)) (keep-if-all pred (cdr lst))))
    ((pred (car lst)) (cons (car lst) (keep-if-all pred (cdr lst))))
    (else (keep-if-all pred (cdr lst)))))
  
;Problem 9
  
(define (list-equal? lst1 lst2)
  (cond
    ((and (null? lst1) (not (null? lst2))) #f)
    ((and (null? lst2) (not (null? lst1))) #f)
    ((and (null? lst1) (null? lst2)) #t)
    ((and (atom? lst1) (atom? lst2)) (eq? lst1 lst2))
    ((and (and (atom? (car lst1)) (atom? (car lst2))) (eq? (car lst1) (car lst2)))(list-equal? (cdr lst1) (cdr lst2)))
    ((eq? (car lst1) (car lst2)) (list-equal1? (cdr lst1) (cdr lst2)))
    ((and (list? (car lst1)) (list? (car lst2))) (and (list-equal? (car lst1) (car lst2)) (list-equal? (cdr lst1) (cdr lst2))))
    (else #f)))

;Problem 10

(define (occurs*? val lst)
  (cond
    ((null? lst) #f)
    ((atom? lst) (eq? val lst))
    (else (or
           (occurs*? val (car lst))
           (occurs*? val (cdr lst))))))

;Problem 11

(define (subst-all gammal ny lst)
  (cond
    ((null? lst) '())
    ((list? (car lst)) (cons (subst-all gammal ny (car lst)) (subst-all gammal ny (cdr lst))))
    ((eq? gammal (car lst)) (cons ny (subst-all gammal ny (cdr lst))))
    (else (cons (car lst) (subst-all gammal ny (cdr lst))))))

;Problem 12

(define (partition pivot lon)
  (define (phelper pivot lon less-or-equal greater)
    (cond
      ((null? lon) (cons less-or-equal greater))
      (( <= (car lon) pivot) (phelper pivot (cdr lon) (cons (car lon) less-or-equal) greater))
      (else (phelper pivot (cdr lon) less-or-equal (cons (car lon) greater)))))
  (phelper pivot lon '() '()))

;Problem 13

;(define (get-first-record lst)
; (car lst))

;(define (get-rest-records lst)
; (cdr lst))

;(define (get-title lst)
; (car lst))

;(define (get-artist lst)
; (car (cdr lst)))
;
;(define (get-year lst)
; (car (cdr (cdr lst))))
;
;(define (get-genre lst)
; (car (cdr (cdr (cdr lst)))))
;
;(define (*the-empty-database*)
; '())
;
;(define (add-record record database)
; (cons record database))
;
;(define (empty-database? database)
; (eq? database *the-empty-database*))
;
;
;Problem 14

(define (get-all field testcase database)
  (if (empty-database? database)
      '()
  (cond
    ((eq? field 'title) (if (list-equal? (get-title (get-first-record database)) testcase)
                            (add-record (get-first-record database) (get-all field testcase (get-rest-records database)))
                            (get-all field testcase (get-rest-records database))))
    ((eq? field 'artist) (if (list-equal? (get-artist (get-first-record database)) testcase)
                            (add-record (get-first-record database) (get-all field testcase (get-rest-records database)))
                            (get-all field testcase (get-rest-records database))))
    ((eq? field 'year) (if (eqv? (get-year (get-first-record database)) testcase)
                            (add-record (get-first-record database) (get-all field testcase (get-rest-records database)))
                            (get-all field testcase (get-rest-records database))))
    ((eq? field 'genre) (if (list-equal? (get-genre (get-first-record database)) testcase)
                            (add-record (get-first-record database) (get-all field testcase (get-rest-records database)))
                            (get-all field testcase (get-rest-records database)))))))

;Problem 15

(define (get-count field testcase database)
 (if (empty-database? database)
      0
  (cond
    ((eq? field 'title) (if (list-equal? (get-title (get-first-record database)) testcase)
                            (+ 1 (get-count field testcase (get-rest-records database)))
                            (get-all field testcase (get-rest-records database))))
    ((eq? field 'artist) (if (list-equal? (get-artist (get-first-record database)) testcase)
                            (+ 1 (get-count field testcase (get-rest-records database)))
                            (get-all field testcase (get-rest-records database))))
    ((eq? field 'year) (if (eqv? (get-year (get-first-record database)) testcase)
                            (+ 1 (get-count field testcase (get-rest-records database)))
                            (get-count field testcase (get-rest-records database))))
    ((eq? field 'genre) (if (list-equal? (get-genre (get-first-record database)) testcase)
                            (+ 1 (get-count field testcase (get-rest-records database)))
                            (get-all field testcase (get-rest-records database)))))))  


(define (count-list lst)
  (if (null? lst)
      0
      (+ 1 (count-list(cdr lst)))))

(define (count-list lst)
  (if (null? lst)
      0
      (+ 1 (count-list(cdr lst)))))
;Problem 16

(define (one-word-entry? entry)
(and (list? entry) (= (count-list entry) 1)))

(define (songs-where field predicate database)
  (if (or (eq? field 'title)
          (eq? field 'artist)
          (eq? field 'year)
          (eq? field 'genre))
  (if (empty-database? database)
      '()
  (cond
    ((eq? field 'title) (if (predicate (get-title (get-first-record database)))
                            (add-record (get-first-record database) (songs-where field predicate (get-rest-records database)))
                            (songs-where field predicate (get-rest-records database))))
    ((eq? field 'artist) (if (predicate (get-artist (get-first-record database)))
                            (add-record (get-first-record database) (songs-where field predicate (get-rest-records database)))
                            (songs-whsubst-all gammalere field predicate (get-rest-records database))))
    ((eq? field 'year) (if (predicate (get-year (get-first-record database)))
                            (add-record (get-first-record database) (songs-where field predicate (get-rest-records database)))
                            (songs-where field predicate (get-rest-records database))))
    ((eq? field 'genre) (if (predicate (get-genre (get-first-record database)))
                            (add-record (get-first-record database) (songs-where field predicate (get-rest-records database)))
                            (songs-where field predicate (get-rest-records database))))))
  (display "Error: Unknown field")))

;Problem 17

(define (get-first-record lst)
  (car lst))

(define (get-rest-records lst)
  (cdr lst))

(define (get-title lst)
  (if (eq? (car (car (cdr lst))) 'title)
      (cdr (car (cdr lst)))
      (get-title (cdr lst))))

(define (get-artist lst)
    (if (eq? (car (car (cdr lst))) 'artist)
      (cdr (car (cdr lst)))
      (get-artist (cdr lst))))

(define (get-year lst)
  (if (eq? (car (car (cdr lst))) 'year)
      (cdr (car (cdr lst)))
      (get-year (cdr lst))))

(define (get-genre lst)
  (if (eq? (car (car (cdr lst))) 'genre)
      (cdr (car (cdr lst)))
      (get-genre (cdr lst))))

(define *the-empty-database*
  '())

(define (add-record record database)
  (list database (subst-all '1 '117 record)))

(define (empty-database? database)
  (eq? database *the-empty-database*))
