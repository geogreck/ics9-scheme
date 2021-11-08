(load "../unit-test.scm")

(define (lil-fix xs)
  (if (list? xs)
      xs
      (list xs)))

(define (derivative-sign-handle expr)
  (if (null? expr)
      '()
      (cons (derivative (lil-fix (car expr))) (derivative-sign-handle (cdr expr)))))

(define (derivative-mult-handle expr)
  (if (number? (car expr))
      (if (> (length expr) 2)
          (list '* (car expr) (derivative (cons '* (cdr expr))))
          (list '* (car expr) (derivative (lil-fix (cadr expr)))))
      (list '+ (list '* (car expr) (derivative (lil-fix (cadr expr))))
            (list '* (derivative (lil-fix (car expr))) (cadr expr)))))
      
(define (derivative-expt-handle expr)
  (if (number? (car expr))
      (list '* expr (list 'log (car expr)))
      (list '* (cadr expr) (list 'expt 'x (- (cadr expr) 1)))))
      
(define (derivative-div-handle expr)
  (list '/ (list '- (list '* (derivative (lil-fix (car expr))) (cadr expr))
                 (list '* (car expr) (derivative (lil-fix (cadr expr)))))
        (list 'expt (cadr expr) 2)))

(define (derivative expr)
  (cond
    ((null? expr) 0)
    ((number? (car expr)) 0)
    ((equal? 'x (car expr)) 1)
    ((or (equal? (car expr) '+) (equal? (car expr) '-)) (cons (car expr) (derivative-sign-handle (cdr expr))))
    ((equal? '* (car expr)) (derivative-mult-handle (cdr expr)))
    ((equal? '/ (car expr)) (derivative-div-handle (cdr expr)))
    ((equal? 'expt (car expr)) (derivative-expt-handle (cdr expr)))
    ((equal? 'exp (car expr)) (list '* expr (derivative (lil-fix (cadr expr)))))
    ((equal? 'cos (car expr)) (list '* (list '- (list 'sin (cadr expr))) (derivative (lil-fix (cadr expr)))))
    ((equal? 'sin (car expr)) (list '* (list 'cos (cadr expr)) (derivative (lil-fix (cadr expr)))))
    ((equal? 'log (car expr)) (list '/ (derivative (lil-fix (cadr expr))) (cadr expr)))
    (else 'iloveanyashipel)))

(define tests1-10
  (list (test (derivative '(2)) 0)
        (test (derivative '(x)) 1)
        (test (derivative '(- x)) (- 1))
        (test (derivative '(- 0 x)) (- 0 1))
        (test (derivative '(- x 1)) (- 1 0))
        (test (derivative '(+ x 1 x)) (+ 1 0 1))
        (test (derivative '(+ x)) (+ 1))
        (test (derivative '(* 1 x)) (* 1 1)) 
        (test (derivative '(* -1 x)) (* -1 1)) 
        (test (derivative '(* -4 x)) (* -4 1)) 
        (test (derivative '(* 10 x)) (* 10 1))
        (test (derivative '(- (* 2 x) 3)) (- (* 2 1) 0))
        (test (derivative (list '* 'x 'x)) (+ (* x 1) (* 1 x)))
        (test (derivative '(expt x 10)) (* 10 (expt x 9)))
        (test (derivative '(* 2 (expt x 5))) (* 2 (* 5 (expt x 4))))));;;;test 10

(define tests11-20
  (list (test (derivative '(expt x -2)) (* -2 (expt x -3)))
        (test (derivative '(expt 5 x)) (* (5 x) (log 5)))
        (test (derivative '(cos x)) (* (- (sin x)) 1))
        (test (derivative '(sin x)) (* (cos x) 1))
        (test (derivative '(exp x)) (* (exp x) 1))
        (test (derivative '(* 2 (exp x))) (* 2 (* (exp x) 1)))
        (test (derivative '(* 2 (exp (* 2 x)))) (* 2 (* (exp (* 2 x)) (* 2 1))))
        (test (derivative '(log x)) (/ 1 x))
        (test (derivative '(* 3 (log x))) (* 3 (/ 1 x)))
        (test (derivative '(+ (expt x 3) (expt x 2))) (+ (* 3 (expt x 2)) (* 2 (expt x 1))))));;;;test 20

(define tests21-30
  (list (test (derivative '(- (* 2 (expt x 3)) (* 2 (expt x 2))))
              (- (* 2 (* 3 (expt x 2))) (* 2 (* 2 (expt x 1)))))
        (test (derivative '(/ 3 x)) (/ (- (* 0 x) (* 3 1)) (expt x 2)))
        (test (derivative '(* 3/2 (expt x -2))) (* 3/2 (* -2 (expt x -3))))
        (test (derivative '(* 2 (sin x) (cos x)))
              (* 2 (+ (* (sin x) (* (- (sin x)) 1)) (* (* (cos x) 1) (cos x)))))
        (test (derivative '(* 2 (exp x) (sin x) (cos x)))
              (* 2 (+ (* (exp x) (* (cos x) 1)) (* (* (exp x) 1) (sin x)))))
        (test (derivative '(sin (* 2 x))) (* (cos (* 2 x)) (* 2 1)))
        (test (derivative '(cos (* 2 (expt x 2))))
              (* (- (sin (* 2 (expt x 2)))) (* 2 (* 2 (expt x 1)))))
        (test (derivative '(sin (log (expt x 2))))
              (* (cos (log (expt x 2))) (/ (* 2 (expt x 1)) (expt x 2))))
        (test (derivative '(+ (sin (* 2 x)) (cos (* 2 (expt x 2)))))
              (+ (* (cos (* 2 x)) (* 2 1)) (* (- (sin (* 2 (expt x 2)))) (* 2 (* 2 (expt x 1))))))
        (test (derivative '(* (sin (* 2 x)) (cos (* 2 (expt x 2)))))
              (+ (* (sin (* 2 x)) (* (- (sin (* 2 (expt x 2)))) (* 2 (* 2 (expt x 1))))) (* (* (cos (* 2 x)) (* 2 1)) (cos (* 2 (expt x 2))))))))
        
;;(run-tests tests1-10)
;;(run-tests tests11-20)
;;(run-tests tests21-30)

(define tests-full
  `(,@tests1-10 ,@tests11-20 ,@tests21-30))

;;(run-tests tests-full)

(load "../trace.scm")

(define-syntax mderivative-1
  (syntax-rules ()
    ((mderivative-1 expr) (eval (list (list 'labmda (list 'x) (car `(,(derivative `expr)))) (cadr `expr)) (interaction-environment)))))

;(eval (list (list 'lambda '(x y) (factorize '(+ (expt x 3) (expt y 3)))) 4 3) (interaction-environment))

(define-syntax mderivative
    (syntax-rules ()
      ((mderivative expr

;;(car `(,(derivative `expr)))
;;(mderivative (expt x 3)) -> ((lambda (y) (* 3 (expt y 2))) x)
;;(mderivative (expt x 3)) -> (* 3 (expt x 2))

(define (der-cube x)
  (mderivative (expt x 3)))

(der-cube 10)
