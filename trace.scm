(define-syntax trace-ex
    (syntax-rules ()
        ((trace-ex var)
         (begin (display (car '(var)))
            (display " => ")
            (display var)
            (newline)
            var))))
