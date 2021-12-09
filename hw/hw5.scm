;соавтор grechkogv@bmstu.ru
;соавтор starovoytovai@bmstu.ru
;;;;;;;;;;;;;;;;;;;;;;;;
;; соавтор аня шипель ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;Я ЛЮБЛЮ АНЮ ШИПЕЛЬ!!!!!!!!!!!!!!!!
;;;;;;;;;;;;;;;;;;;;;;;;14.1

(define feature-if-else #t)
(define feature-nested-if #t)
(define feature-while-loop #t)
(define feature-repeat-loop #t)
(define feature-for-loop #t)
(define feature-global #t)
(define feature-tail-call #t)
(define feature-hi-level #t)

;; -> stack после выполнения program
(define (interpret program stack)
  (let ((jmps (make-vector (vector-length program))))
    (begin
      ;; предподсчитаем за O(len(program)) индексы соответствий:
      ;; if else endif: if -> else + 1, else -> endif + 1
      ;; if endif: if -> endif + 1
      ;; while wend: while -> wend + 1, wend -> while
      ;; repeat until: repeat + 1 <- until
      ;; for next: for -> next + 1, next -> for + 1
      ;; и сохраним их в вектор jmps[i] = индекс соответствующего слова
      (let loop ((index 0)
                 (stack '()))
        (if (< index (vector-length program))
            (let ((command (vector-ref program index)))
              (case command
                (if (loop (+ index 1) (cons index stack)))
                (endif (begin (vector-set! jmps (car stack) (+ 1 index))
                              (loop (+ index 1) (cdr stack))))
                (while (loop (+ index 1) (cons index stack)))
                (wend (begin (vector-set! jmps (car stack) (+ 1 index))
                             (vector-set! jmps index (car stack))
                             (loop (+ index 1) (cdr stack))))
                (repeat (loop (+ index 1) (cons index stack)))
                (until (begin (vector-set! jmps index (+ 1 (car stack)))
                              (loop (+ index 1) (cdr stack))))
                (for (loop (+ index 1) (cons index stack)))
                (next (begin (vector-set! jmps index (+ 1 (car stack)))
                             (vector-set! jmps (car stack) (+ index 1))
                             (loop (+ index 1) (cdr stack))))
                (lam (loop (+ index 1) (cons index stack)))
                (endlam (begin (vector-set! jmps (car stack) (+ 1 index))
                               (loop (+ index 1) (cdr stack))))
                (else (if (eqv? command 'else)
                          (begin (vector-set! jmps (car stack) (+ 1 index))
                                 (loop (+ index 1) (cons index (cdr stack))))
                          (loop (+ index 1) stack)))))))
      (let interpret-internal ((stack stack)
                               (index 0)
                               (dictionary '())
                               (for-stack '()))
        (if (= index (vector-length program))
            stack
            (let ((command (vector-ref program index)))
              ;; выполнить операции со стеком, перейти к выполнению следующего слова
              (let-syntax ((call-next (syntax-rules ()
                                        ((_ new-stack)
                                         (interpret-internal new-stack
                                                             (+ index 1)
                                                             dictionary
                                                             for-stack)))))
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
                      (& (interpret-internal (cons (cadr (assoc (vector-ref program (+ index 1)) dictionary))
                                                   stack)
                                             (+ index 2)
                                             dictionary
                                             for-stack))
                      (apply (let ((jmp-index (car stack)))
                                      (call-next (interpret-internal (cdr stack)
                                                                     jmp-index
                                                                     dictionary
                                                                     for-stack))))
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
                                                        dictionary)
                                                  for-stack))
                      (defvar (interpret-internal stack
                                                  (+ index 3)
                                                  (cons (list (vector-ref program (+ index 1))
                                                              'var
                                                              (vector-ref program (+ index 2)))
                                                        dictionary)
                                                  for-stack))
                      (set (interpret-internal (cdr stack)
                                               (+ index 2)
                                               (set-cdr! (cdr (assoc (vector-ref program (+ index 1)) dictionary)) (list (car stack)))
                                               for-stack))
                      (end stack)
                      (exit stack)
                      (endlam stack)
                      (lam (interpret-internal (cons (+ index 1) stack)
                                               (vector-ref jmps index)
                                               dictionary
                                               for-stack))
                      (if (if (= 0 (car stack))
                              ;; выполняем вторую ветку, если она есть
                              (interpret-internal (cdr stack)
                                                  (vector-ref jmps index)
                                                  dictionary
                                                  for-stack)
                              ;; выполняем первую ветку
                              ;; если встретится else - скипнется вторая ветка
                              ;; если endif - выполняем следующее слово
                              (call-next (cdr stack))))
                      (endif (call-next stack))
                      (while (if (= 0 (car stack))
                                 ;; скипаем тело while
                                 (interpret-internal (cdr stack)
                                                     (vector-ref jmps index)
                                                     dictionary
                                                     for-stack)
                                 ;; выполняем тело while
                                 (call-next (cdr stack))))
                      ;; вернемся до соответствующего while
                      (wend (interpret-internal stack
                                                (vector-ref jmps index)
                                                dictionary
                                                for-stack))
                      (repeat (call-next stack))
                      (until (if (= 0 (car stack))
                                 (call-next (cdr stack))
                                 (interpret-internal (cdr stack)
                                                     (vector-ref jmps index)
                                                     dictionary
                                                     for-stack)))
                      (for (let ((from (cadr stack))
                                 (to (car stack)))
                             (if (> from to)
                                 (interpret-internal (cddr stack)
                                                     (vector-ref jmps index)
                                                     dictionary
                                                     for-stack)
                                 (interpret-internal (cddr stack)
                                                     (+ index 1)
                                                     dictionary
                                                     (cons from (cons to for-stack))))))
                      (next (let ((i (+ 1 (car for-stack))))
                              (if (> i (cadr for-stack))
                                  (interpret-internal stack
                                                      (+ index 1)
                                                      dictionary
                                                      (cddr for-stack))
                                  (interpret-internal stack
                                                      (vector-ref jmps index)
                                                      dictionary
                                                      (cons i (cdr for-stack))))))
                      (tail (let ((jmp-index (cadr (assoc (vector-ref program (+ index 1)) dictionary))))
                              (interpret-internal stack
                                                  jmp-index
                                                  dictionary
                                                  for-stack)))
                      (i (call-next (cons (car for-stack) stack)))
                      ;; в case нельзя использовать else, поэтому обработаю его тут
                      (else (if (eqv? command 'else)
                                (interpret-internal stack
                                                    (vector-ref jmps index)
                                                    dictionary
                                                    for-stack)
                                ;; ищем статью в словаре
                                (if (eqv? (cadr (assoc command dictionary)) 'var)
                                    (call-next (cons (caddr (assoc command dictionary)) stack))
                                    (let ((jmp-index (cadr (assoc command dictionary))))
                                      (call-next (interpret-internal stack
                                                                     jmp-index
                                                                     dictionary
                                                                     for-stack)))))))))))))))

