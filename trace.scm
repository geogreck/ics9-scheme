(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex var)
     (let ((res var))
       (begin (write (car '(var)))
              (display " => ")
              (write res)
              (newline)
              res)))))

(define-syntax trace-ex-silent
  (syntax-rules ()
    ((trace-ex-silent var)
     (begin (write (car '(var)))
            (display " => ")
            (write var)
            (newline)
            ))))
