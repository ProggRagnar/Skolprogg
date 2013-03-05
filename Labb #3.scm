(load "interaction-utils.scm")
(load "table-helper.scm")
(require (lib "trace.ss"))

;Problem 1

(define (for-each-element procedure list)
  (if (not (null? list))
      (begin
        (procedure (car list))
        (for-each-element procedure (cdr list)))))


;Problem 2

(define (rev list)
  (let ((res '())) ; introduce a local variable for the result
    (define (loop)
      (if (null? list)
          res
          (begin
            (set! res (cons (car list) res))
            (set! list (cdr list))
            (loop))))
    (loop)))

;Problem 3

(define count-calls
  (let ((count 0))
    (lambda lst
      (cond
        ((null? lst) (begin
                       (set! count (+ count 1))
                       count))
        ((eq? (car lst) 'how-many-calls) count)
        ((eq? (car lst) 'reset-count) (set! count 0) count)))))

;Problem 4 - Se bifogat blad

;Problem 5

(define (make-monitored fn)
  (let ((count 0))
    (lambda lst
      (cond
        ((null? lst) (fn))
        ((eq? (car lst) 'how-many-calls) count)
        ((eq? (car lst) 'reset) (set! count 0))
        (else (begin
                (set! count (+ count 1))
                (apply fn lst)))))))

;Problem 6 - Se bifogat blad

;Problem 7
