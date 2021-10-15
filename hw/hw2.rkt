;;;;;;;; Task 1
(define (my-range a b d)
  (if (< a b)
      (cons a (my-range (+ a d) b d))
      '()))

(define (my-flatten xs)
  (cond ((null? xs) '())
        ((list? xs) (append (my-flatten (car xs)) (my-flatten (cdr xs))))
        (else (cons xs '()))))

(define (my-flatten1 xs)
  (define (loop res xs)
    (if (null? xs)
        res
        (if (list? xs)
            (loop (loop res (cdr xs)) (car xs))
            (cons xs res))))
  (loop '() xs))
        

(define (my-element? x xs)
  (or (equal? (car xs) x) (and (> (length xs) 1) (my-element? x (cdr xs)))))

(define (my-filter pred? xs)
  (cond ((null? xs) '())
        ((pred? (car xs)) (cons (car xs)
                                (my-filter pred? (cdr xs))))
        (else (my-filter pred? (cdr xs)))))

(define (my-reassamble op xs)
  (append (cons (op (car xs) (cadr xs)) '()) (cddr xs)))

(define (my-fold-left op xs)
  (if (> (length xs) 1)
      (my-fold-left op (my-reassamble op xs))
      (car xs)))

(define (my-rev-reassamble op xs)
  (reverse (append (cons (op (cadr (reverse xs)) (car (reverse xs))) '()) (cddr (reverse xs)))))

(define (my-fold-right op xs)
  (if (> (length xs) 1)
      (my-fold-right op (my-rev-reassamble op xs))
      (car xs)))

;;;;;;;;Task 2

(define (count x xs)
  (if (and (not (null? xs)) (equal? (car xs) x))
      (if (> (length xs) 1)
          (+ 1 (count x (cdr xs)))
          1)
      (if (> (length xs) 1)
          (count x (cdr xs))
          0)))

(define (list->set xs)
  (cond ((null? xs) '())
        ((= (count (car xs) xs) 1) (cons (car xs) (list->set (cdr xs))))
        (else (list->set (cdr xs)))))

(define (set? xs)
  (equal? xs (list->set xs)))

(define (union xs ys)
  (if (null? xs)
      ys
      (if (= (count (car xs) ys) 1)
          (union (cdr xs) ys)
          (cons (car xs) (union (cdr xs) ys)))))

(define (intersection xs ys)
  (cond ((null? xs) '())
        ((= 1 (count (car xs) ys)) (cons (car xs) (intersection (cdr xs) ys)))
        (else (intersection (cdr xs) ys))))

(define (difference xs ys)
  (cond ((null? xs) '())
        ((= 0 (count (car xs) ys)) (cons (car xs) (difference (cdr xs) ys)))
        (else (difference (cdr xs) ys))))

(define (symmetric-difference xs ys)
  (my-flatten (cons (difference xs ys) (difference ys xs))))

(define (set-eq1? xs ys)
  (and (or (null? xs) (= 1 (count (car xs) ys))) (or (null? xs) (set-eq1? (cdr xs) ys))))

(define (set-eq? xs ys)
  (and (set-eq1? xs ys) (set-eq1? ys xs)))

;;;;; Task 3
(define (cut-first-string a)
  (list->string (cdr (string->list a))))

(define (string-trim-left a)
  (if (and (not(equal? "" a))
           (char-whitespace? (string-ref a 0)))
      (string-trim-left (cut-first-string a))
      a))

(define (cut-last-string a)
  (list->string(reverse (cdr (reverse (string->list a))))))

(define (string-trim-right a)
  (if (and (not(equal? "" a))
           (char-whitespace? (string-ref a (- (string-length a) 1))))
      (string-trim-right (cut-last-string a))
      a))

(define (string-trim a)
  (string-trim-right (string-trim-left a)))

(define (string-prefix? a b)
  (cond ((equal? a "") #t)
        ((equal? a b) #t)
        ((>= (string-length a) (string-length b)) #f)
        ((equal? (string-ref a 0) (string-ref b 0)) (string-prefix? (cut-first-string a) (cut-first-string b)))
        (else #f)))

(define (reverse-string a)
  (list->string (reverse (string->list a))))

(define (string-suffix? a b)
  (string-prefix? (reverse-string a) (reverse-string b)))

(define (string-infix? a b)
  (cond ((equal? a "") #t)
        ((equal? a b) #t)
        ((>= (string-length a) (string-length b)) #f)
        ((equal? (string-ref a 0) (string-ref b 0)) (string-infix? (cut-first-string a) (cut-first-string b)))
        (else (string-infix? a (cut-first-string b)))))

(define (cut-n-string a n)
  (if (> n 0)
      (cut-n-string (cut-first-string a) (- n 1))
      a))

(define (string-split str sep)
  (cond ((equal? str "") '())
        ((string-prefix? sep str) '())))

(define (string-split str sep)
  (define (loop lstr buff)
    (if (null? lstr)
        (list (list->string buff))
        (if (string-prefix? sep (list->string lstr))
            (cons (list->string buff)
                  (loop (string->list (cut-n-string (list->string lstr) (string-length sep)))
                        '()))
            (loop (cdr lstr) (append buff (list (car lstr)))))))
  (loop (string->list str) '()))


;;;;;Task 4

(define (make-multi-vector sizes . fill)
  (vector "multi-vector-structure" sizes (if (null? fill)
                                             (make-vector (apply * sizes))
                                             (make-vector (apply * sizes) (car fill)))))

(define (multi-vector? m)
  (equal? (vector-ref m 0) "multi-vector-structure"))

(define (lists-multiply indices sizes)
  (if (= (length indices) 1)
      (car indices)
      (+ (* (car indices)
            (apply * (cdr sizes)))
         (lists-multiply (cdr indices) (cdr sizes)))))

(define (multi-vector-ref m indices)
  (vector-ref (vector-ref m 2) (lists-multiply indices (vector-ref m 1))))
  
        
(define (multi-vector-set! m indices x)
  (vector-set! (vector-ref m 2) (lists-multiply indices (vector-ref m 1)) x))

(define m (make-multi-vector '(11 12 9 16)))
(multi-vector? m)
(multi-vector-set! m '(10 7 6 12) 'test)
(multi-vector-ref m '(10 7 6 12))
(multi-vector-set! m '(1 2 1 1) 'X)
(multi-vector-set! m '(2 1 1 1) 'Y)
(multi-vector-ref m '(1 2 1 1))
(multi-vector-ref m '(2 1 1 1))
(define m (make-multi-vector '(3 5 7) -1))
(multi-vector-ref m '(0 0 0))

(define z (make-multi-vector '(2 2)))
z
(multi-vector-set! z '(1 0) 'a)
(multi-vector-set! z '(0 1) 'b)
z
(multi-vector-ref z '(0 1))
(multi-vector-ref z '(1 0))

;;;;;;;;;Testi

(display "Task 1\n")
(my-range  0 11 3)
(my-flatten '((1) 2 (3 (4 5)) 6))
(my-element? 1 '(3 2 1))
(my-element? 4 '(3 2 1))
(my-filter odd? (my-range 0 10 1))
(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1))
(my-fold-left  quotient '(16 2 2 2 2))
(my-fold-left  quotient '(1))
(my-fold-right expt     '(2 3 4))
(my-fold-right expt     '(2))

(display "\nTask 2\n")
(list->set '(1 1 2 3))
(set? '(1 2 3))
(set? '(1 2 3 3))
(set? '())
(union '(1 2 3) '(2 3 4))
(intersection '(1 2 3) '(2 3 4))
(difference '(1 2 3 4 5) '(2 3))
(symmetric-difference '(1 2 3 4) '(3 4 5 6))
(set-eq? '(1 2 3) '(3 2 1))
(set-eq? '(1 2) '(1 3))

(list #\a #\b #\space #\c #\tab #\space)
