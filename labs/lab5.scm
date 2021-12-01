;соавтор grechkogv@bmstu.ru
;соавтор starovoytovai@bmstu.ru
;;;;;;;;;;;;;;;;;;;;;;;;
;; соавтор аня шипель ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; -> stack после выполнения program
(define (interpret program stack)
  (let interpret-internal ((program program)
                           (stack stack)
                           (index 0)
                           (dictionary '()))
    (if (= index (vector-length program))
        stack
        (let ((command (vector-ref program index))
              (arithmetic? (lambda (command)
                             (member command '(+ - * / mod neg nigger))))
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
                          (member command '(> < =))))
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
                        (member command '(not and or))));;ахахахахах типо лоджик вейп))))
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
                             (member command '(drop swap dup over rot depth))))
              (stack-anya (lambda (command stack)
                            (cond
                             ((equal? 'drop command) (cdr stack))
                             ((equal? 'swap command) (cons (cadr stack) (cons (car stack) (cddr stack))))
                             ((equal? 'dup command) (cons (car stack) stack))
                             ((equal? 'over command) (cons (cadr stack) stack))
                             ((equal? 'rot command) (cons (caddr stack) (cons (cadr stack) (cons (car stack) (cdddr stack)))))
                             ((equal? 'depth command)(cons (length stack) stack)))))
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
                 (interpret-internal program
                                     (cons command stack)
                                     (+ index 1)
                                     dictionary))
                ((arithmetic? command)
                 (interpret-internal program
                                     (arithmetic command stack)
                                     (+ index 1)
                                     dictionary))
                ((compare? command)
                 (interpret-internal program
                                     (compare command stack)
                                     (+ index 1)
                                     dictionary))
                ((logic? command)
                 (interpret-internal program
                                     (logic command stack)
                                     (+ index 1)
                                     dictionary))
                ((stack-anya? command)
                 (interpret-internal program
                                     (stack-anya command stack)
                                     (+ index 1)
                                     dictionary))
                ((equal? 'define command)
                 (let* ((new-proc (definition))
                        (val (vector-length (cadr new-proc))))
                   (interpret-internal program
                                       stack
                                       (+ index val 3)
                                       (cons new-proc dictionary))))
                ((assoc command dictionary) => (lambda (called-proc)
                                                 (interpret-internal program
                                                                     (interpret-internal (cadr called-proc)
                                                                                         stack
                                                                                         0
                                                                                         dictionary)
                                                                     (+ index 1)
                                                                     dictionary)))
                ((equal? 'exit command) stack)
                ((equal? 'if command)
                 (let* ((body (iffish))
                        (val (vector-length body)))
                   (if (= 0 (car stack))
                       (interpret-internal program
                                           (cdr stack)
                                           (+ index val 2)
                                           dictionary)
                       (interpret-internal program
                                           (cdr stack)
                                           (+ index 1)
                                           dictionary))))
                ((equal? 'endif command)
                 (interpret-internal program
                                     stack
                                     (+ index 1)
                                     dictionary)))))))
