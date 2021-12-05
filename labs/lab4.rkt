
;;;;;;Task 1

(define point
  #f)

(define cal/cc call-with-current-continuation)

(define (use-assertions)
  (call-with-current-continuation
                       (lambda (cc) (set! point cc))))

(define cal/cc call-with-current-continuation)

(define-syntax assert
  (syntax-rules ()
    ((assert expr) (if (not expr)
                       (begin
                         (display "FAILED: ")
                         (write 'expr)
                         (newline)
                         (point))))))

(use-assertions)

(define (1/x x)
  (assert (not (zero? x))) ; Утверждение: x ДОЛЖЕН БЫТЬ ≠ 0
  (/ 1 x))


;(map 1/x '(1 2 3 4 5))
;(map 1/x '(-2 -1 0 1 2))

;;;; Task2

(define (save-data data filename)
  (with-output-to-file filename (lambda () (begin
                                             (write data)
                                             (newline)))))
(define (load-data filename)
  (with-input-from-file filename (lambda () (begin
                                              (let ((s (read)))
                                                s)))))

(define (count-lines filename)
  (with-input-from-file filename (lambda ()
                                   (let loop ((prev_s #f) (lines-count 1))
                                     (let ((s (read-char)))
                                       (cond ((eof-object? s) lines-count)
                                             ((and (not(equal? prev_s #\newline)) (equal? #\newline s)) (loop s (+ 1 lines-count)))
                                             (else (loop s lines-count))))))))
      
                                              

;(define x 100)

;;;; Task 3


(define (trib1 n)
  (cond ((< n 2) 0)
        ((= n 2) 1)
        (else (+ (trib1 (- n 1)) (trib1 (- n 2)) (trib1 (- n 3))))))

(define (trib n)
  (let ((memo (make-vector n 'empty)))
    (let loop ((i n))
      (cond ((< i 2) 0)
            ((= i 2) 1)
            (else (if (equal? (vector-ref memo (- i 1)) 'empty)
                      (vector-set! memo  (- i 1) (+ (loop (- i 1))
                                                    (loop (- i 2))
                                                    (loop (- i 3)))))
                  (vector-ref memo (- i 1)))))))

;(display (trib 40))
;(newline)
;(display (trib1 40))
;(newline)

;;;; Task 4

(define-syntax my-if
  (syntax-rules ()
    ((my-if expr t-branch f-branch) (let ((t-prom (delay t-branch))
                                          (f-prom (delay f-branch)))
                                      (force (or (and expr t-prom) f-prom))))))

;(my-if #t 1 (/ 1 0))
;(my-if #f (/ 1 0) 1)

;;;; Task 5

(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var val)) body) ((lambda (var) body) val))
    ((my-let ((var val) . xs) body) (my-let xs ((lambda (var) body) val)))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* ((var val)) body) ((lambda (var) body) val))
    ((my-let* ((var val) . xs) body) (my-let ((var val))
                                             (my-let* (xs) body)))))

;(my-let ((x 2) (y 3))
;  (* x y))

;;;; Task 6

(define-syntax when
  (syntax-rules ()
    ((when cond? one-action) (and cond? one-action))
    ((when cond? one-action . actions) (and cond? (let ((x one-action))
                                                    (when cond? . actions))))))

(define x 1)
;(when   (> x 0) (display "x > 0") (newline))

(define-syntax unless
  (syntax-rules ()
    ((unless cond? one-action) (and (not cond?) one-action))
    ((unless cond? one-action . actions) (and (not cond?) (let ((x one-action))
                                                            (unless cond? . actions))))))

;(unless (= x 0) (display "x != 0") (newline))

;for
(define-syntax for
  (syntax-rules (in as)
    ((for x in xs . actions) (let loop ((n (length xs)) (i 0) (xs1 xs))
                               (if (not (null? xs1)) (let ((x (car xs1)))
                                                       (if (< i n)
                                                           (begin
                                                             (begin . actions)
                                                             (loop n (+ i 1) (cdr xs1))))))))
    ((for xs as x . actions) (let loop ((n (length xs)) (i 0) (xs1 xs))
                               (if (not (null? xs1)) (let ((x (car xs1)))
                                                       (if (< i n)
                                                           (begin
                                                             (begin . actions)
                                                             (loop n (+ i 1) (cdr xs1))))))))))


;(for i in '(1 2 3)
; (for j in '(4 5 6)
; (display (list i j))
;(newline)))

;(newline)

;(for '(1 2 3) as i
; (for '(4 5 6) as j
;  (display (list i j))
; (newline)))


;while

(define-syntax while
  (syntax-rules ()
    ((while cond? . actions) (letrec ((loop (lambda ()
                                              (if cond?
                                                  (begin
                                                    (begin . actions)
                                                    (loop)))))) (loop)))))

;(let ((p 0)
;     (q 0))
;(while (< p 3)
;      (set! q 0)
;     (while (< q 3)
;          (display (list p q))
;          (newline)
;         (set! q (+ q 1)))
; (set! p (+ p 1))))

;repeat.....until

(define-syntax repeat
  (syntax-rules (until)
    ((repeat actions until cond?) (letrec ((loop (lambda ()
                                                   (begin
                                                     (begin . actions)
                                                     (if (not cond?)
                                                         (loop)))))) (loop)))))

;(let ((i 0)
;     (j 0))
;(repeat ((set! j 0)
;        (repeat ((display (list i j))
;                (set! j (+ j 1)))
;              until (= j 3))
;     (set! i (+ i 1))
;    (newline))
;  until (= i 3)))

;cout

(define-syntax cout
  (syntax-rules ()
    ((cout . exprs) (let loop ((xs `exprs))
                      (cond ((null? xs) (values))
                            ((equal? (car xs) '<<) (loop (cdr xs)))
                            ((equal? (car xs) 'endl) (begin
                                                       (newline)
                                                       (loop (cdr xs))))
                            (else (begin
                                    (display (car xs))
                                    (loop (cdr xs)))))))))

;(cout << "a = " << 1 << endl << "b = " << 2 << endl)                                               
