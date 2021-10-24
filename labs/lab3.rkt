;;;;;;;; Task1
(load "../trace.scm")

(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss)))) ; Здесь...
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss)))))) ; ... и здесь

(define (my-reassamble op xs)
  (append (cons (op (car xs) (cadr xs)) '()) (cddr xs)))

(define (my-fold-left op xs)
  (if (> (length xs) 1)
      (my-fold-left op (my-reassamble op (trace-ex xs)))
      (car xs)))

;;(my-fold-left  quotient '(16 2 2 2 2))

;;;;;;;; Task2

(load "../unit-test.scm")

(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1) ; Ошибка здесь!
    (else     1)))

(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))
      
;;;;; Task3

(define (ref-string xs args)
  (if (= (length args) 1)
      (and (< (car args) (string-length xs))
           (string-ref xs (car args)))
      (and (<= (car args) (string-length xs))
           (char? (cadr args))
           (string-append (substring xs 0 (car args))
                          (make-string 1 (cadr args))
                          (substring xs (car args) (string-length xs))))))

(define (ref-list xs args)
  (if (= (length args) 1)
      (and (< (car args) (length xs))
           (list-ref xs (car args)))
      (and (<= (car args) (length xs))
            `(,@(reverse (list-tail (reverse xs) (- (length xs) (car args)))) ,@`(,(cadr args)) ,@(list-tail xs (car args))))))

(define (ref-vector xs args)
  (if (= (length args) 1)
      (vector-ref xs (car args))
      (list->vector (ref-list (vector->list xs) args))))
      

(define (ref xs . args)
  (cond ((null? args) #f)
        ((string? xs) (ref-string xs args))
        ((list? xs) (ref-list xs args))
        ((vector? xs) (ref-vector xs args))
        (else #f)))

(define tests-ref
  (list
   (test (ref '(1 2 3) 1)  2)
   (test (ref #(1 2 3) 1)  2)
   (test (ref "123" 1)     #\2)
   (test (ref "123" 3)     #f)
   (test (ref '(1 2 3) 1 0)    (1 0 2 3))
   (test (ref #(1 2 3) 1 0)    #(1 0 2 3))
   (test (ref #(1 2 3) 1 #\0)  #(1 #\0 2 3))
   (test (ref "123" 1 #\0)     "1023")
   (test (ref "123" 1 0)       #f)
   (test (ref "123" 3 #\4)     "1234")
   (test (ref "123" 5 #\4)     #f)))

;(trace-ex-silent (ref "123" 3))
;(trace-ex-silent (ref "123" 3 #\0))
;(trace-ex-silent (ref '(1 2 3) 1 0))
;(trace-ex-silent (ref '(1 2 3) 1 #\0))
;(trace-ex-silent (ref #(1 2 3) 1 #\0))
;;(trace-ex (ref #(1 2 3) 1 0))

;;;;;; Task4

(define factorize-tests
  (list (test (factorize '(- (expt x 2) (expt y 2)))
              (* (- x y) (+ x y)))
        (test (factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))
              (* (- (+ first 1) (- second 1)) (+ (+ first 1) (- second 1))))
        (test (factorize '(+ (expt x 3) (expt y 3)))
              (* (+ x y) (+ (expt x 2) (expt y 2) (- (* x y)))))
        (test (factorize '(- (expt x 3) (expt y 3)))
              (* (- x y) (+ (expt x 2) (expt y 2) (* x y))))
        (test (factorize '(+ (expt (+ first 1) 3) (expt (- second 1) 3)))
              (* (+ (+ first 1) (- second 1)) (+ (expt (+ first 1) 2) (expt (- second 1) 2) (- (* (+ first 1) (- second 1))))))
        (test (eval (list (list 'lambda '(x y) (factorize '(- (expt x 2) (expt y 2)))) 1 2) (interaction-environment))
              -3)
        (test (eval (list (list 'lambda '(x y) (factorize '(- (expt x 3) (expt y 3)))) 4 3) (interaction-environment))
              37)
        (test (eval (list (list 'lambda '(x y) (factorize '(+ (expt x 3) (expt y 3)))) 4 3) (interaction-environment))
              91)
        (test (eval (list (list 'lambda '(x y) (factorize '(+ (expt x 3) (expt y 3)))) 4 3) (interaction-environment))
              90)))

(define (factorize expression)
  (if (= (car (cddadr expression)) 2)
      `(* (- ,@`(,(cadadr expression)) ,@`(,(car (cdaddr expression))))
          (+ ,@`(,(cadadr expression)) ,@`(,(car (cdaddr expression)))))
      (if (equal? (car expression) '-)
          `(* (- ,@`(,(cadadr expression)) ,@`(,(car (cdaddr expression))))
              (+  (expt ,@`(,(cadadr expression)) 2) (expt ,@`(,(car (cdaddr expression))) 2) (* ,@`(,(cadadr expression)) ,@`(,(car (cdaddr expression))))))
          `(* (+ ,@`(,(cadadr expression)) ,@`(,(car (cdaddr expression))))
              (+  (expt ,@`(,(cadadr expression)) 2) (expt ,@`(,(car (cdaddr expression))) 2) (- (* ,@`(,(cadadr expression)) ,@`(,(car (cdaddr expression))))))))))
  