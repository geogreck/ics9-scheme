;соавтор grechkogv@bmstu.ru
;соавтор starovoytovai@bmstu.ru
;;;;;;;;;;;;;;;;;;;;;;;;
;; соавтор аня шипель ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; - [X] реализуйте if с альтернативной веткой,
;; - [X] реализуйте вложенные if,
;; - [ ] циклы (с предусловием, с постусловием, с параметром),
;; - [ ] выход из цикла и переход к новой итерации (break/continue),
;; - [ ] конструкцией switch-case,
;; - [ ] безымянными функциями и косвенным вызовом функций,
;; - [ ] хвостовую рекурсию,
;; - [ ] глобальные переменные в пространстве имен, отдельном от функций и пр.

;; -> stack после выполнения program
(define (interpret program stack)
  (let interpret-internal ((stack stack)
                           (index 0)
                           (dictionary '()))
    (if (= index (vector-length program))
        stack
        (let ((command (vector-ref program index)))
          ;; выполнить операции со стеком, перейти к выполнению следующего слова
          (let-syntax ((call-next (syntax-rules ()
                                    ((_ new-stack)
                                     (interpret-internal new-stack
                                                         (+ index 1)
                                                         dictionary)))))
            (if (number? command)
                (call-next (cons command stack))
                (case command
                  (+ (call-next (cons (+ (cadr stack) (car stack))
                                      (cddr stack))))
                  (- (call-next (cons (- (cadr stack) (car stack))
                                      (cddr stack))))
                  (* (call-next (cons (* (cadr stack) (car stack))
                                      (cddr stack))))
                  (/ (call-next (cons (quotient (cadr stack) (car stack))
                                      (cddr stack))))
                  (mod (call-next (cons (remainder (cadr stack) (car stack))
                                        (cddr stack))))
                  (neg (call-next (cons (- (car stack))
                                        (cdr stack))))
                  (= (call-next (cons (if (= (cadr stack) (car stack))
                                          -1
                                          0)
                                      (cddr stack))))
                  (< (call-next (cons (if (< (cadr stack) (car stack))
                                          -1
                                          0)
                                      (cddr stack))))
                  (> (call-next (cons (if (> (cadr stack) (car stack))
                                          -1
                                          0)
                                      (cddr stack))))
                  (not (call-next (cons (if (equal? (car stack) 0)
                                            -1
                                            0)
                                        (cdr stack))))
                  (and (call-next (cons (if (and (not (equal? (car stack) 0))
                                                 (not (equal? (cadr stack) 0)))
                                            -1
                                            0)
                                        (cddr stack))))
                  (or (call-next (cons (if (or (not (equal? (car stack) 0))
                                               (not (equal? (cadr stack) 0)))
                                           -1
                                           0)
                                       (cddr stack))))
                  (drop (call-next (cdr stack)))
                  (swap (call-next (cons (cadr stack)
                                         (cons (car stack) (cddr stack)))))
                  (dup (call-next (cons (car stack) stack)))
                  (over (call-next (cons (cadr stack) stack)))
                  (rot (call-next (cons (caddr stack)
                                        (cons (cadr stack)
                                              (cons (car stack) (cdddr stack))))))
                  (depth (call-next (cons (length stack) stack)))
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
                          ;; выполняем вторую ветку, если она есть
                          (interpret-internal (cdr stack)
                                              ;; скипаем слова до
                                              ;; соответствующего по вложенности endif или else
                                              ;; +1 (продолжаем выполнение со следующего слова)
                                              ;; это что, ПСП?
                                              (+ 1
                                                 (let loop ((index (+ index 1))
                                                            (balance 0))
                                                   (let ((command (vector-ref program index)))
                                                     (case command
                                                       (if (loop (+ index 1) (+ balance 1)))

                                                       (endif (if (= balance 0)
                                                                  index
                                                                  (loop (+ index 1) (- balance 1))))
                                                       ;; в case нельзя использовать else,
                                                       ;; поэтому обработаю его тут
                                                       (else (if (eqv? 'else command)
                                                                 (if (= balance 0)
                                                                     index
                                                                     (loop (+ index 1) balance))
                                                                 (loop (+ index 1) balance)))))))
                                              dictionary)
                          ;; выполняем первую ветку
                          ;; если встретится else - скипнется вторая ветка
                          ;; если endif - выполняем следующее слово
                          (call-next (cdr stack))))
                  (endif (call-next stack))
                  ;; в case нельзя использовать else, поэтому обработаю его тут
                  (else (if (eqv? command 'else)
                            (interpret-internal stack
                                                (+ 1
                                                   (let loop ((index (+ index 1))
                                                              (balance 0))
                                                     (case (vector-ref program index)
                                                       (if (loop (+ index 1) (+ balance 1)))
                                                       (endif (if (= balance 0)
                                                                  index
                                                                  (loop (+ index 1) (- balance 1))))
                                                       (else (loop (+ index 1) balance)))))
                                                dictionary)
                            ;; ищем статью в словаре
                            (let ((jmp-index (cadr (assoc command dictionary))))
                              (call-next (interpret-internal stack
                                                             jmp-index
                                                             dictionary))))))))))))
