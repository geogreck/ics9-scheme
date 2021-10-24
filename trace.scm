(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex var)
     (begin (write (car '(var)))
            (display " => ")
            (write var)
            (newline)
            var))))

(define-syntax trace-ex-silent
  (syntax-rules ()
    ((trace-ex-silent var)
     (begin (write (car '(var)))
            (display " => ")
            (write var)
            (newline)
            ))))