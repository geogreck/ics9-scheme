
(load "./lab5.scm")
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
        (test (interpret #(define -- 1 - end
                           5 -- --)
                         '())
              (3))
        (test (interpret #(10 15 +
                           define -- 1 - end
                           exit
                           5 -- --)
                         '())
              (25))
        (test (interpret #(10 15 +
                           define -- exit 1 - end
                           5 -- --)
                         '())
              (5 25))
        (test (interpret #(10 4 dup) '()) (4 4 10))
        (test (interpret #(define abs
                           dup 0 <
                           if neg endif
                           end
                           9 abs
                           -9 abs
                           10 abs
                           -10 abs)
                         '())
              (10 10 9 9))

        (test (interpret #(define =0? dup 0 = end
                           define <0? dup 0 < end
                           define signum
                           =0? if exit endif
                           <0? if drop -1 exit endif
                           drop
                           1
                           end
                           0 signum
                           -5 signum
                           10 signum)
                         '())
              (1 -1 0))

        (test (interpret #(define -- 1 - end
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
                           4 factorial)
                         '())
              (24 6 2 1 1))

        (test (interpret #(define =0? dup 0 = end
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
                           10 make-fib)
                         '())
              (0 1 1 2 3 5 8 13 21 34 55))

        (test (interpret #(define =0? dup 0 = end
                           define gcd
                           =0? if drop exit endif
                           swap over mod
                           gcd
                           end
                           90 99 gcd
                           234 8100 gcd)
                         '())
              (18 9))
        (test (interpret #(define =0? dup 0 = end =0?) '(0)) (-1 0))
        (test (interpret #(define =0? dup 0 = end
                           define kek 0 =0? end
                           kek)
                         '())
              (-1 0))))

(run-tests tests)
