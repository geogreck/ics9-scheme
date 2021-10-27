(define-syntax test
  (syntax-rules ()
    ((test action ans)
     (list (car '(action)) '(ans)))))

(define (run-test test)
  (begin
    (write (car test))
  (if (equal? (eval (car test) (interaction-environment))
              (caadr test))
      (begin
        (display " ok\n")
        #t)
      (begin
        (display " FAIL\n")
        (display "  Expected: ")
        (write (caadr test))
        (newline)
        (display "  Returned: ")
        (write (eval (car test) (interaction-environment)))
        (newline)
        #f))))

(define (and-fold xs)
  (or (null? xs)
      (and (car xs)
           (and-fold (cdr xs)))))

(define (run-tests xs)
  (if (or (equal? (car xs) #f) (equal? (car xs) #t))
      (and-fold xs)
      (run-tests (append (cdr xs) `(,(run-test (car xs)))))))
