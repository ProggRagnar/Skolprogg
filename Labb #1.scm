
;Problem 1

(define foo
  (* 1 2 3 4))

(define (foobar)
  (* 1 2 3 4))

;foo - returnerar 24, foo blir en konstant.
;foobar - säger #<procedure:foobar>. Dvs talar om att foobar är en procedur
;(foo) - Försöker köra en procedur av namnet foo men får en konstant.
;(foobar) - 24. (foobar) är en procedur. När den körs fås värdet 24.


;Problem 2

;(define (sum n) 
;(* n (/ (+ 1 n) 2)))



;(define (sum n)
;  (if (= n 0)
;      0
;      (+ n (sum (- n 1)))))



(define (sum n)
  (define (sum-help n res)
    (if(= n 1)
       res
       (sum-help (- n 1) (+ res n))))
  (if (= n 0)
      0
  (sum-help n 1)))



;Problem 3


;(define (powers-of-two n)
;  (if (= n 0)
;      1
;      ( * 2 (powers-of-two (- n 1)))))


;Problem 4

;(define (pascal rad kol)
;  (cond
;    ((= 1 kol) 1)
;    ((= rad kol) 1)
;    (else (+ (pascal (- rad 1) kol) (pascal (- rad 1) (- kol 1))))))




;Problem 5


(define (my-* num1 num2)
  (if (= num2 0)
      0
      (+ num1 (my-* num1 (- num2 1)))))


(define (double num)
  (* num 2))

(define (halve num) 
  (/ num 2))


(define (multi num1 num2)
  (cond
    ((= num2 0) 0)
    ((even? num2) (multi (* num1 2) (/ num2 2)))
    (else (+ num1 (multi num1 (- num2 1))))))


;Problem 6

(define (last-digit n)
  (remainder n 10))

(define (but-last-digit n)
  (quotient n 10))

(define (sum-of-digits n)
  (if (= n 0) 0 
      (+ (last-digit n) (sum-of-digits (but-last-digit n)))))



(define (number-of-digits n)
  (if (= n 0) 0
      (+ 1 (number-of-digits (but-last-digit n)))))



(define (divisible? t n)
  (if (= (remainder t n) 0) #t #f))



(define (random-from-to from to)
  (+ from (random (- (+ to 1) from))))




;Problem 7


(define (simple-sv-num? tal nämnare)
  (divisible? (sum-of-digits tal) nämnare))



(define (make-simple-sv-num nämnare)
  (let ((slump (random-from-to 100000 999999)))
    (if (divisible? (sum-of-digits slump) nämnare)
        slump 
        (make-simple-sv-num nämnare))))


;Problem 8


(define (make-cc-sv-sum)
  (let ((slump (random-from-to 100000 999999)))
    (define (helper slump)
      (if (= slump 0) 
          0
          (+ (cond 
               ((odd? (number-of-digits slump)) (last-digit slump))
               ((>= (last-digit slump) 5) (+ (* (last-digit slump) 2) 1))
               ((< (last-digit slump) 5) (* (last-digit slump) 2)))
             (helper (but-last-digit slump)))))
    (if (divisible? (helper slump) 10)
        slump
        (make-cc-sv-sum))))


;Problem 9


(define (sum-and-apply-to-digits number digit-proc)
  (if (= (number-of-digits number) 1)
      (digit-proc number 1)
      (+ (digit-proc (last-digit number) (number-of-digits number))
         (sum-and-apply-to-digits (but-last-digit number) digit-proc))))


(define (number-of-digits2 number)
  (sum-and-apply-to-digits number (lambda (digit pos) 1)))


(define (sum-of-digits2 number)
  (sum-and-apply-to-digits number (lambda (digit pos) digit)))



(define (make-cc-sv-num2)
  (let ((slump (random-from-to 100000 999999)))
  (if (divisible? (sum-and-apply-to-digits slump prochelper) 10)
      slump
      (make-cc-sv-num2))))

(define (prochelper digit pos)
  (cond
    ((odd? pos) digit)
    ((> 5 digit) (* 2 digit))
    ((+ (* 2 digit) 1))))

;Problem 10

;(define (personnummer-helper nummer position)
;  (if (even? position)
;      nummer
;      (sum-of-digits (* nummer 2))))

;(define (personnummer nummer)
;(if (divisible? (sum-and-apply-to-digits nummer personnummer-helper) 10)
;    #t
;    #f))
