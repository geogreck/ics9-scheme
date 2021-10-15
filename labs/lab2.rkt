
;;;;;;; Task 1
(define (count x xs)
  (if (and (not (null? xs)) (equal? (car xs) x))
      (if (> (length xs) 1)
          (+ 1 (count x (cdr xs)))
          1)
      (if (> (length xs) 1)
          (count x (cdr xs))
          0)))

;;;;;;;; Task 2
(define (delete pred? xs)
  (cond ((null? xs) '())
        ((pred? (car xs)) (append (delete pred? (cdr xs))))
        (else (append (cons (car xs)
                            (delete pred? (cdr xs)))))))

;;;;;; Task 3
(define (iterate f x n)
  (if (>= n 1)
       (append (cons x (iterate f (f x) (- n 1))))
        '()))

;;;;; Task 4
(define (intersperse e xs)
  (if (> (length xs) 1)
      (append (append (list (car xs) e) (intersperse e (cdr xs))))
      (append xs)))

;;;;; Task 5
(define (any? pred? xs)
  (or (and (not (null? xs)) (pred? (car xs)))
      (and (> (length xs) 1) (any? pred? (cdr xs)))))

(define (all? pred? xs)
  (or (null? xs)
      (and (and (> (length xs) 0) (pred? (car xs)))
           (or (= (length xs) 1) (and (> (length xs) 1) (any? pred? (cdr xs)))))))

;;;;;; Task 6
(define (o . xs)
  (lambda (x)
    (cond ((> (length xs) 1) ((car xs) ((apply o (cdr xs)) x)))
          ((= (length xs) 1) ((car xs) x))
          (else x))))

(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

;;;;;;;; Examples
(display "Task 1\n")
(count 'a '(a b c a))
(count 'b '(a c d))
(count 'a '())
(newline)

(display "Task 2\n")
(delete even? '(0 1 2 3))
(delete even? '(0 2 4 6))
(delete even? '(1 3 5 7))
(delete even? '())
(newline)

(display "Task 3\n")
(iterate (lambda (x) (* 2 x)) 1 6)
(iterate (lambda (x) (* 2 x)) 1 1)
(iterate (lambda (x) (* 2 x)) 1 0)
(newline)

(display "Task 4\n")
(intersperse 'x '(1 2 3 4))
(intersperse 'x '(1 2))
(intersperse 'x '(1))
(intersperse 'x '())
(newline)

(display "Task 5\n")
(any? odd? '(1 3 5 7))
(any? odd? '(0 1 2 3))
(any? odd? '(0 2 4 6))
(any? odd? '())
(newline)
(all? odd? '(1 3 5 7))
(all? odd? '(0 1 2 3))
(all? odd? '(0 2 4 6))
(all? odd? '())
(newline)

(display "Task 6\n")
((o f g h) 1)
((o f g) 1)
((o h) 1)
((o) 1)