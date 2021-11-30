;;соавтор аня шипель

(load "../trace.scm")

(define (contains? elem list)
  (let loop ((list list))
    (and (not (null? list))
         (or (equal? (car list) elem)
             (loop (cdr list))))))

(define mod remainder)

;; -> stack после выполнения program
(define (interpret-sub program stack dictionary)
  (let loop ((index 0)
             (dictionary dictionary)
             (stack stack))
    (if (= index (vector-length program))
        stack
        (let ((command (vector-ref program index))
              (arithmetic? (lambda (command)
                             (contains? command '(+ - * / mod neg nigger))))
              (arithmetic (lambda (command stack)
                            (cond
                             ((equal? command '+) (cons (+ (cadr stack) (car stack)) (cddr stack)))
                             ((equal? command '-) (cons (- (cadr stack) (car stack)) (cddr stack)))
                             ((equal? command '*) (cons (* (cadr stack) (car stack)) (cddr stack)))
                             ((equal? command '/) (cons (quotient (cadr stack) (car stack)) (cddr stack)))
                             ((equal? command 'mod) (cons (remainder (cadr stack) (car stack)) (cddr stack)))
                             ((equal? command 'neg) (cons (- (car stack)) (cdr stack)))
                             (else 'ILOVEANYASHIPEL))))
              (compare? (lambda (command)
                          (contains? command '(> < =))))
              (compare (lambda (command stack)
                         (cons (cond
                                ((equal? command '=) (if (= (cadr stack) (car stack))
                                                         -1
                                                         0))
                                ((equal? command '>) (if (> (cadr stack) (car stack))
                                                         -1
                                                         0))
                                ((equal? command '<) (if (< (cadr stack) (car stack))
                                                         -1
                                                         0)))
                               (cddr stack))))
              (logic? (lambda (command)
                        (contains? command '(not and or))));;ахахахахах типо лоджик вейп))))
              (logic (lambda (command stack)
                     (cond
                            ((equal? 'not command) (cons (if (equal? (car stack) 0)
                                                             -1
                                                             0)
                                                         (cdr stack)))
                            ((equal? 'and command) (cons (if (and (not (equal? (car stack) 0)) (not (equal? (cadr stack) 0)))
                                                             -1
                                                             0)
                                                         (cddr stack)))
                            ((equal? 'or command) (cons (if (or (not (equal? (car stack) 0)) (not (equal? (cadr stack) 0)))
                                                            -1
                                                            0)
                                                        (cddr stack))))))
              (stack-anya? (lambda (command)
                             (contains? command '(drop swap dup over rot depth))))
              (stack-anya (lambda (command stack)
                            (cond
                             ((equal? 'drop command) (cdr stack))
                             ((equal? 'swap command) (cons (cadr stack) (cons (car stack) (cddr stack))))
                             ((equal? 'dup command) (cons (car stack) stack))
                             ((equal? 'over command) (cons (cadr stack) stack))
                             ((equal? 'rot command) (cons (caddr stack) (cons (cadr stack) (cons (car stack) (cdddr stack)))))
                             ((equal? 'depth command)(cons (length stack) stack)))))
              (definition? (lambda (command)
                         (equal? 'define command)))
              (definition (lambda () ;; -> '(word #())
                            (list (vector-ref program (+ index 1))
                                  (let loop ((index (+ index 2))
                                             (res '()))
                                    (let ((current (vector-ref program index)))
                                      (if (equal? 'end current)
                                          (list->vector (reverse res))
                                          (loop (+ index 1) (cons current res))))))))
              (iffish (lambda () ;; -> #()
                        (let loop ((index (+ index 1))
                                   (res '()))
                          (let ((current (vector-ref program index)))
                            (if (equal? 'endif current)
                                (list->vector (reverse res))
                                (loop (+ index 1) (cons current res))))))))
          (cond ((number? command)
                 (loop (+ index 1)
                       dictionary
                       (cons command stack)))
                ((arithmetic? command)
                 (loop (+ index 1)
                       dictionary
                       (arithmetic command stack)))
                ((compare? command)
                 (loop (+ index 1)
                       dictionary
                       (compare command stack)))
                ((logic? command)
                 (loop (+ index 1)
                       dictionary
                       (logic command stack)))
                ((stack-anya? command)
                 (loop (+ index 1)
                       dictionary
                       (stack-anya command stack)))
                ((definition? command) ;; TODO убрать definition?
                 (let* ((new-proc (definition))
                        (val (vector-length (cadr new-proc))))
                   (loop (+ index val 3)
                         (cons new-proc dictionary)
                         stack)))
                ((assoc command dictionary) => (lambda (called-proc)
                                                 (loop (+ index 1)
                                                       dictionary
                                                       (interpret-sub(trace-ex (cadr called-proc)) stack dictionary))))
                ((equal? 'exit command) stack)
                ((equal? 'if command)
                 (let* ((body (iffish))
                        (val (vector-length body)))
                   (if (= 0 (car stack))
                       (loop (+ index val 2)
                             dictionary
                             (cdr stack))
                       (loop (+ index 1)
                             dictionary
                             (cdr stack)))))
                ((equal? 'endif command)
                 (loop (+ index 1)
                       dictionary
                       stack)))))))

(define (interpret program stack)
  (interpret-sub program stack '()))

(load "../unit-test.scm")

(define tests
  (list (test (interpret #(1) '()) (1))
        (test (interpret #(1 2) '(10)) (2 1 10))
        (test (interpret #(1 2 +) '(10)) (3 10))
        (test (interpret #(1 2 -) '(10)) (-1 10))
        (test (interpret #(1 2 *) '(10)) (2 10))
        (test (interpret #(1 2 /) '(10)) (0 10))
        (test (interpret #(1 2 mod) '(10)) (1 10))
        (test (interpret #(1 2 neg) '(10)) (-2 1 10))
        (test (interpret #(2 3 * 4 5 * +) '()) (26))
        (test (interpret #(10 10 =) '()) (-1))
        (test (interpret #(10 0 >) '()) (-1))
        (test (interpret #(0 10 <) '()) (-1))
        (test (interpret #(10 5 =) '()) (0))
        (test (interpret #(0 10 >) '()) (0))
        (test (interpret #(10 0 <) '()) (0))
        (test (interpret #(0 0 and) '()) (0))
        (test (interpret #(1000 7 -) '()) (993))
        (test (interpret #(100 100 and) '()) (-1))
        (test (interpret #(100 0 or) '()) (-1))
        (test (interpret #(100 not) '()) (0))
        (test (interpret #(0 not) '()) (-1))
        (test (interpret #(   define -- 1 - end
                            5 -- --      )  '()) (3))
        (test (interpret #(10 15 +
                              define -- 1 - end
                              exit
                              5 -- --) '()) (25))
        (test (interpret #(10 15 +
                              define -- exit 1 - end
                              5 -- --) '()) (5 25)) ;; TODO списать лабу
        (test (interpret #(10 4 dup) '()) (4 4 10))
  (test (interpret #(   define abs
                      dup 0 <
                      if neg endif
                  end
                   9 abs
                   -9 abs
                   10 abs
                   -10 abs) (quote ()))
     (10 10 9 9))

  (test (interpret #(   define =0? dup 0 = end
                  define <0? dup 0 < end
                  define signum
                      =0? if exit endif
                      <0? if drop -1 exit endif
                      drop
                      1
                  end
                   0 signum
                  -5 signum
                  10 signum       ) (quote ()))
     (1 -1 0))

  (test (interpret #(   define -- 1 - end
                  define =0? dup 0 = end
                  define =1? dup 1 = end
                  define factorial
                      =0? if drop 1 exit endif
                      =1? if drop 1 exit endif
                      dup --
                      factorial
                      *
                  end
                  0 factorial
                  1 factorial
                  2 factorial
                  3 factorial
                  4 factorial     ) (quote ()))
     (24 6 2 1 1))

  (test (interpret #(   define =0? dup 0 = end
                  define =1? dup 1 = end
                  define -- 1 - end
                  define fib
                      =0? if drop 0 exit endif
                      =1? if drop 1 exit endif
                      -- dup
                      -- fib
                      swap fib
                      +
                  end
                  define make-fib
                      dup 0 < if drop exit endif
                      dup fib
                      swap --
                      make-fib
                  end
                  10 make-fib     ) (quote ()))
     (0 1 1 2 3 5 8 13 21 34 55))

  (test (interpret #(   define =0? dup 0 = end
                  define gcd
                      =0? if drop exit endif
                      swap over mod
                      gcd
                  end
                  90 99 gcd
                  234 8100 gcd    ) '())
     (18 9))
  (test (interpret #(define =0? dup 0 = end =0?) '(0)) (-1 0))
  (test (interpret #(define =0? dup 0 = end define kek 0 =0? end kek) '()) (-1 0))
        ))

(run-tests tests)
