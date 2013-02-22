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
