;;;;;;Task extra

(define (cut-vector xs n)
  (define (loop i k)
    (if (>= k -1)
        (cons (vector-ref xs i)
              (loop (+ i 1) (- k 1)))
        '()))
  (loop 0 n))
     

(define (list-trim-right xs)
  (define (loop ys n)
    (if (and (char? (vector-ref ys n))
             (char-whitespace? (vector-ref ys n)))
        (loop ys (- n 1))
        (cut-vector ys (- n 1))))
  (loop (list->vector xs) (- (length xs) 1)))

(list-trim-right '(1 2 3 #\space #\tab))
(list-trim-right '(1 2 '(1 2 3) #\space #\tab))

(define (foo f x)
  (f x))

(define (my-rev-reassamble op xs)
  (reverse (append (cons (op (cadr (reverse xs)) (car (reverse xs))) '()) (cddr (reverse xs)))))

(define (my-fold-right op xs)
  (if (> (length xs) 1)
      (my-fold-right op (my-rev-reassamble op xs))
      (car xs)))

(define (o . xs)
  (lambda (x)
    (my-fold-right foo (append xs (cons x '())))))

(define (f x)
  (+ x 2))
(define (g x)
  (* x 3))
(define (h x)
  (- x))

((o f g h) 1)
((o f g) 1)
