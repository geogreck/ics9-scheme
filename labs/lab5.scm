;соавтор grechkogv@bmstu.ru
;соавтор starovoytovai@bmstu.ru
;;;;;;;;;;;;;;;;;;;;;;;;
;; соавтор аня шипель ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; -> stack после выполнения program
(define (interpret program stack)
  (let interpret-internal ((stack stack)
                           (index 0)
                           (dictionary '()))
    (if (= index (vector-length program))
        stack
        (let ((command (vector-ref program index)))
          ;; выполнить операции со стеком, перейти к выполнению следующего слова
          (let-syntax ((nxt-w-new-st (syntax-rules ()
                                       ((_ new-stack)
                                        (interpret-internal new-stack
                                                            (+ index 1)
                                                            dictionary)))))
            (if (number? command)
                (nxt-w-new-st (cons command stack))
                (case command
                  (+ (nxt-w-new-st (cons (+ (cadr stack) (car stack))
                                         (cddr stack))))
                  (- (nxt-w-new-st (cons (- (cadr stack) (car stack))
                                         (cddr stack))))
                  (* (nxt-w-new-st (cons (* (cadr stack) (car stack))
                                         (cddr stack))))
                  (/ (nxt-w-new-st (cons (quotient (cadr stack) (car stack))
                                         (cddr stack))))
                  (mod (nxt-w-new-st (cons (remainder (cadr stack) (car stack))
                                           (cddr stack))))
                  (neg (nxt-w-new-st (cons (- (car stack))
                                           (cdr stack))))
                  (= (nxt-w-new-st (cons (if (= (cadr stack) (car stack))
                                             -1
                                             0)
                                         (cddr stack))))
                  (< (nxt-w-new-st (cons (if (< (cadr stack) (car stack))
                                             -1
                                             0)
                                         (cddr stack))))
                  (> (nxt-w-new-st (cons (if (> (cadr stack) (car stack))
                                             -1
                                             0)
                                         (cddr stack))))
                  (not (nxt-w-new-st (cons (if (equal? (car stack) 0)
                                               -1
                                               0)
                                           (cdr stack))))
                  (and (nxt-w-new-st (cons (if (and (not (equal? (car stack) 0))
                                                    (not (equal? (cadr stack) 0)))
                                               -1
                                               0)
                                           (cddr stack))))
                  (or (nxt-w-new-st (cons (if (or (not (equal? (car stack) 0))
                                                  (not (equal? (cadr stack) 0)))
                                              -1
                                              0)
                                          (cddr stack))))
                  (drop (nxt-w-new-st (cdr stack)))
                  (swap (nxt-w-new-st (cons (cadr stack)
                                            (cons (car stack) (cddr stack)))))
                  (dup (nxt-w-new-st (cons (car stack) stack)))
                  (over (nxt-w-new-st (cons (cadr stack) stack)))
                  (rot (nxt-w-new-st (cons (caddr stack)
                                           (cons (cadr stack)
                                                 (cons (car stack) (cdddr stack))))))
                  (depth (nxt-w-new-st (cons (length stack) stack)))
                  (define (interpret-internal stack
                                              (+ 1 ;; следующее слово за end
                                                 (let loop ((index (+ index 2))) ;; вернет индекс следующего end
                                                   (if (eqv? 'end (vector-ref program index))
                                                       index
                                                       (loop (+ index 1)))))
                                              (cons (list
                                                     (vector-ref program (+ index 1))
                                                     (+ index 2)) ;; (название-статьи индекс-первого-слова-статьи)
                                                    dictionary)))
                  (end stack)
                  (exit stack)
                  (if (if (= 0 (car stack))
                          (interpret-internal (cdr stack)
                                              (+ 1
                                                 (let loop ((index (+ index 1)))
                                                   (if (eqv? 'endif (vector-ref program index))
                                                       index
                                                       (loop (+ index 1)))))
                                              dictionary)
                          (nxt-w-new-st (cdr stack))))
                  (endif (nxt-w-new-st stack))
                  (else (let ((jmp-index (cadr (assoc command dictionary))))
                          (nxt-w-new-st (interpret-internal stack
                                                            jmp-index
                                                            dictionary)))))))))))
