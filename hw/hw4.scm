(define (memoized-fib n)
  (let ((known-results (list '(1 0) '(2 1))))
    ((lambda (n)
       (let* ((args n)
              (res (assoc args known-results)))
         (if res
             (cadr res)
             (let ((res (+ (memoized-factorial (- n 1)) (memoized-factorial (- n 2)))))
               (set! known-results (cons (list args res) known-results))
               res)))) n)))

(define (memoized-factorial n)
  (let ((known-results (list '(0 1))))
    ((lambda (n)
       (let* ((args n)
              (res (assoc args known-results)))
         (if res
             (cadr res)
             (let ((res (* n (memoized-factorial (- n 1)))))
               (set! known-results (cons (list args res) known-results))
               res)))) n)))

(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (lazy-car lazy-pair)
  (if (pair? lazy-pair)
      (car lazy-pair)
      lazy-pair))

(define (lazy-cdr lazy-pair)
  (if (pair? lazy-pair)
      (force (cdr lazy-pair))))

(define pair1 (lazy-cons (+ 1 2) (+ 3 4)))
(define list1 (lazy-cons (* 10 5) pair1))
;(display pair1)
;(newline)
;(display list1)
;(newline)
;(display (lazy-cdr list1))
;(newline)
;(display (lazy-cdr (lazy-cdr list1)))
;(newline)

(define (lazy-head xs k)
  (let loop ((xss xs) (n k))
    (if (> n 0)
        (cons (lazy-car xss) (loop (lazy-cdr xss) (- n 1)))
        '())))

(define (lazy-ref xs k)
  (let loop ((xss xs) (n 1))
    (if (not (= n k))
        (loop (lazy-cdr xss) (+ n 1))
        (lazy-car xss))))

(define (naturals start)
  (lazy-cons start (naturals (+ start 1))))

(define (read-words)
  (let loop ((buf "") (s (read-char)))
    (if (eof-object? s)
        '()
        (if (char-whitespace? s)
            (if (not (equal? buf ""))
                (cons buf (loop "" (read-char)))
                (loop "" (read-char)))
            (loop (string-append buf (string s)) (read-char))))))

;;;task 4
(define (get-fields fields)
  (let loop ((xs fields))
    (if (not (null? xs))
        (cons (list (string->symbol (string-append "field-" (symbol->string (car xs)))) (car xs)) (if (not (null? (cdr xs)))
                                                                                                      (loop (cdr xs))
                                                                                                      '())))))

(define-syntax define-struct
  (syntax-rules ()
    ((define-struct struct-name (field1 ...))
     (begin
       (eval `(define ,(string->symbol (string-append "make-" (symbol->string `struct-name)))
                (lambda (field1 ...)
                  `(,(string->symbol (string-append "struct-" (symbol->string `struct-name)))
                    ,`(field1 ,field1) ...))) (interaction-environment))
       (eval `(define ,(string->symbol (string-append (symbol->string `struct-name) "?"))
                (lambda (struct)
                  (and (list? struct)
                       (equal? (car struct)
                               ',(string->symbol (string-append "struct-" (symbol->string `struct-name))))))) (interaction-environment))
       (eval `(define ,(string->symbol (string-append (symbol->string 'struct-name) "-" (symbol->string 'field1)))
                (lambda (struct)
                  (cadr (assoc 'field1 (cdr struct))))) (interaction-environment))
       ...
       (eval `(define ,(string->symbol (string-append "set-" (symbol->string 'struct-name) "-" (symbol->string 'field1) "!"))
                (lambda (struct val)
                  (set-car! (cdr (assoc 'field1 (cdr struct))) val))) (interaction-environment))
       ...
       ))))

(define-syntax define-data
  (syntax-rules ()
    ((define-data name ((variant1 field1 ...) ...))
     (begin
       (eval `(define variant1
                (lambda (field1 ...)
                  `(,`(data-name name) ,`(variant-name variant1) ,`(field1 ,field1) ...))) (interaction-environment))
       ...
       (eval `(define ,(string->symbol (string-append (symbol->string `name) "?"))
                (lambda (data)
                  (and (list? data)
                       (equal? (cadr (assoc 'data-name data)) `name)))) (interaction-environment))))))

(define-syntax match
  (syntax-rules ()
    ((match f
       ((name field1 ...) formula) ...) (begin
                                          (cond ((equal? `name (cadr (assoc 'variant-name f)))
                                                 ((lambda (field1 ...)
                                                    formula) (cadr (assoc `field1 f)) ...))
                                                ...)))))
      
                

(define-data figure ((square a)
                     (rectangle a b)
                     (triangle a b c)
                     (circle r)))

(define s (square 10))
(define r (rectangle 10 20))
(define t (triangle 10 20 30))
(define c (circle 10))

(define pi (acos -1)) ; Для окружности

(define (perim f)
  (match f 
    ((square a)       (* 4 a))
    ((rectangle a b)  (* 2 (+ a b)))
    ((triangle a b c) (+ a b c))
    ((circle r)       (* 2 pi r))))

(perim s)
(perim c)

;;:;extra
;;task 2
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var val)) body) ((lambda (var) body) val))
    ((my-let ((var val) . xs) body) (my-let xs ((lambda (var) body) val)))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* ((var val)) body) ((lambda (var) body) val))
    ((my-let* ((var val) . xs) body) (my-let ((var val))
                                             (my-let* (xs) body)))))
;;task 1
(((call-with-current-continuation
   (lambda (c) c))
  (lambda (x) x))
 'hello)
;В переменную с кладется контекст, который сразу же вызывается и возвращает lambda от x, которая получает на вход 'hello и просто возвращает его же

;;task 3
;;Как мне кажется, так будет работать не всегда, так как это DrRacket производит вычисления слева направо.
;;Соответственно, в первом случае в функцию суммирования сначала попадет значение x, а потом уже будет запомнен контекст вычислений, с постоянным значением x
;;Во втором же случае сначала будет запомнем контекст, а потом будет произведен вызов значения x, а так как значение x каждый раз меняется с помощью set!,
;;то при каждом возврате к контексту значение x будет новое