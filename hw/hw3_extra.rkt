(load "../unit-test.scm")

;;;;Task 2
(define-syntax flatten
  (syntax-rules ()
    ((flatten lst)
     (eval (let loop ((list `lst) (res '()))
       (cond
         ((null? list) res)
         ((pair? list) (loop (car list) (loop (cdr list) res)))
         (else (cons list res)))) (interaction-environment)))))

(flatten (((+) 1) (2 (3) ((4)))))

      
;;; Task 1

(define (check-sum xs)
  (if (null? xs)
      '()
      (if (equal? (car xs) 0)
          (check-sum (cdr xs))
          (cons (car xs) (check-sum (cdr xs))))))

(define (check-mult xs)
  (if (null? xs)
      '()
      (if (equal? (car xs) 1)
          (check-mult (cdr xs))
          (if (equal? (car xs) 0)
              '(annihilate)
              (cons (car xs) (check-mult (cdr xs)))))))
   

(define (simplify xs)
  (cond
    ((equal? '+ (car xs)) (let* ((res (check-sum xs)) (len (length res)))
                            (cond ((= len 1) 0)
                                  ((= len 2) (cadr res))
                                  (else res))))
    ((equal? '* (car xs)) (let* ((res (check-mult xs)) (len (length res)) (tail (list-ref res (- len 1))))
                            (cond ((equal? tail 'annihilate) 0)
                                  ((= len 2) (cadr res))
                                  (else res))))
    (else xs)))

(define simplify-tests
  (list (test (simplify '(+ 1 2 x 0)) (+ 1 2 x))
        (test (simplify '(+ 1 12)) (+ 1 12))
        (test (simplify '(+ 10 0 0)) 10)
        (test (simplify '(+ 0 0)) 0)
        (test (simplify '(* 10 0 100 x)) 0)
        (test (simplify '(* 10 1)) 10)
        (test (simplify '(* 10 100)) (* 10 100))))
