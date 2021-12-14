(load "../trace.scm")

;<fraction> ::= <sign> <number> / <number>
;<sign> ::= + | - | <empty>
;<empty> ::=
;<number> ::= <digit> <number-tail>
;<number-tail> ::= <digit> <number-tail> | <empty>
;<digit> ::= 1| 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 0

(define (get-sign xs)
  (list 'sign
        (if (equal? (car xs) #\-)
            '-
            '+)))
(define (clear-sign xs)
  (if (or (equal? (car xs) #\-) (equal? (car xs) #\+))
      (cdr xs)
      xs))

(define (get-tokens xs)
  (let ((sign (get-sign xs)) (numerator '()) (denominator '()))
    (let loop ((xss (clear-sign xs)) (flag #t))
      (if (not (null? xss))
          (if (equal? (car xss) #\/)
              (if flag
                  (loop (cdr xss) #f)
                  (set! numerator '()))
              (if (char-numeric? (car xss))
                  (begin
                    (if flag
                        (set! numerator (cons (car xss) numerator))
                        (set! denominator (cons (car xss) denominator)))
                    (loop (cdr xss) flag))
                  (set! numerator '())))))
    (list sign (list 'numerator (reverse numerator)) (list 'denominator (reverse denominator)))))
                  
(define (check-frac frac)
  (let ((tokens (get-tokens (string->list frac))))
    (and (not (null? (cadr (assoc 'numerator tokens))))
         (not (null? (cadr (assoc 'denominator tokens)))))))

(define (check-frac-tokens frac-tokens)
  (and (not (null? (cadr (assoc 'numerator frac-tokens))))
       (not (null? (cadr (assoc 'denominator frac-tokens))))))

(define (scan-frac frac)
  (let ((tokens (get-tokens (string->list frac))))
    (and (check-frac-tokens tokens)
         (* (if (equal? '+ (cadr (assoc 'sign tokens)))
                1
                -1)
            (/ (string->number (list->string (cadr (assoc 'numerator tokens))))
               (string->number (list->string (cadr (assoc 'denominator tokens)))))))))

;<many-fracs> ::= <fraction> <many-fracs> | <spaces> <many-fracs> | <spaces>
;<spaces> ::= <space> <spaces> | <empty>
;<space> ::= space | tab | line feed | form feed | carriage return

(define (scan-many-fracs fracs)
  (let loop ((buf '()) (xs (string->list (string-append fracs " "))) (fracs '()))
    (if (not (null? xs))
        (let ((s (car xs)))
          (if (char-whitespace? s)
              (if (null? buf)
                  (loop '() (cdr xs) fracs)
                  (let ((frac (scan-frac (list->string buf))))
                    (if frac
                        (loop '() (cdr xs) (cons frac fracs))
                        (loop '() '() #f))))
              (loop (append buf (list s)) (cdr xs) fracs)))
        (and fracs (reverse fracs)))))

;;;; Task 2

(define exit #f)
(define call/cc call-with-current-continuation)


;<Program>  ::= <Articles> <Body> .
;<Articles> ::= <Article> <Articles> | .
;<Article>  ::= define word <Body> end .
;<Body>     ::= if <Body> endif <Body> | integer <Body> | word <Body> | .

(define (keyword? x)
  (member x '(if endif define)))

(define (if-tail program)
  (let loop ((program program) (depth -1))
    (if (null? program)
        #f
        (let ((s (car program)))
          (cond
            ((and (equal? s 'endif) (zero? depth)) (cdr program))
            ((equal? s 'endif) (loop (cdr program) (- depth 1)))
            ((equal? s 'if) (loop (cdr program) (+ depth 1)))
            (else (loop (cdr program) depth)))))))

(define (parse-body program)
  (let loop ((program program) (parsed '()) (buf '()))
    (if (not (null? program))
        (let ((s (car program)))
          (case s
            (if (let ((tail (if-tail program)))
                  (if tail
                      (loop tail (append parsed (list (list 'if (loop (cdr program) '() (cons 'if buf))))) buf))))
            (endif (if (and (not (null? buf))
                            (equal? 'if (car buf)))
                       parsed
                       (exit #f)))
            (define (exit #f))
            (end (exit #f))
            (else (loop (cdr program) (append parsed (list s)) buf))))
        parsed)))
                  
              

(define (parse-article program)
  (let loop ((program program) (buf '()))
    (if (null? program)
        (exit #f)
        (if  (equal? (car program) 'end)
             (parse-body (reverse buf))
             (loop (cdr program) (cons (car program) buf))))))

(define (rm-article program)
  (let loop ((program program))
    (if (equal? 'end (car program))
        (cdr program)
        (loop (cdr program)))))
  
(define (parse-articles program)
  (let loop ((program program))
    (if (null? program)
        (list program)    
        (let ((s (car program)))
          (if (equal? s 'define)
              (if (null? (cdr program))
                  (exit #f)
                  (if (keyword? (cadr program))
                      (exit #f)
                      (let ((article (parse-article (cddr program))))
                        (if article
                            (cons (cons (cadr program) (list article))
                                  (loop (rm-article program)))))))
              (list program))))))

(define (parse program)
  (call/cc
   (lambda (cc)
     (begin
       (set! exit cc)
       (let* ((programl (vector->list program))
              (articles (parse-articles programl))
              (main-body (parse-body (car (list-tail articles (- (length articles) 1)))))
              (only-articles (reverse (list-tail (reverse articles) 1))))
         (list only-articles main-body))))))

(parse #(define abs 
          dup 0 < 
          if neg endif 
          end 
          9 abs 
          -9 abs))

(parse #(1 2 +))

(parse #(define word w1 w2 w3))

(parse #( define -- 1 - end
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
           4 factorial ))
(parse #(define =0? dup 0 = end
          define gcd
          =0? if drop exit endif
          swap over mod
          gcd
          end
          90 99 gcd
          234 8100 gcd))
(parse #(1 2 if + if dup - endif endif dup))