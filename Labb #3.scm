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

(define (convert-loop fn)
  (display "Give me a number!")
  (let ((user-input (read)))
    (cond
      ((eq? user-input 'quit) (display "Bye bye!"))
      ((not (number? user-input)) (display user-input) (display " is not a number. Try again!"))                   
      (else (display user-input) (display "--->") (display (fn user-input))))))

;Problem 8

(define (install-my-utils)
  (define (double num) (* 2 num))
  (define (triple num) (* 3 num))
  (begin
    (put 'doubling 'my-utils double)
    (put 'tripling 'my-utils triple)
    (display "ok")))
                         

(define (install-your-utils)
  (define (double num) (+ num num))
  (begin
    (put 'doubling 'your-utils double)
    (put 'tripling 'your-utils (lambda (num) (+ num num num)))
    (display "ok")))

(define (double num)
  ((get 'doubling 'your-utils) num))

(define (triple num)
  ((get 'tripling 'your-utils) num))

;Problem 9

(define (interaction-loop)
  (display ">")
  (let ((user-input (enter-new-command!)))
  (cond
    ((eq? (get-command-name) 'quit) (display "Bye bye!"))
    ((get 'lab-3.9 (get-command-name)) (apply (get 'lab-3.9 (get-command-name)) 
                                                   (get-command-arguments)))
    (else (display "Unknown command:  ") (display (get-command-name))))))

(define (add-command! name funk)
  (put 'lab-3.9 name funk)
  'ok)
  
