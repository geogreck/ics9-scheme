
(load "./hw5.scm")
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
              (-1 0))
        (test (interpret #(define =0? dup 0 = end
                          dup
                          =0?
                          if
                              dup
                              =0?
                              if
                                115
                              endif
                              20
                          endif) '(0))
              (20 115 0 0 0))
        (test (interpret #(define =0? dup 0 = end
                          dup
                          =0?
                          if
                              dup
                              =0?
                              0
                              if
                                115
                              else
                                230
                              endif
                              20
                          endif) '(0))
              (20 230 -1 0 0 0))
        (test (interpret #(define =0? dup 0 = end
                            10
                            =0? if
                                115
                                0
                                    if =0?
                                        100
                                    endif
                            else
                                300
                                1
                                =0? if
                                    4000
                                else
                                    700
                                endif
                            endif) '())
              (700 1 300 10))
        (test (interpret #(define -- 1 - end
                            4 dup while -- dup wend) '())
              (0))))

(define feature-if-else
  (list (test (interpret #(1 if 100 else 200 endif) '())
              (100))
        (test (interpret #(0 if 100 else 200 endif) '())
              (200))))

(define feature-nested-if
  (list (test (interpret #(0 if 1 if 2 endif 3 endif 4) '())
              (4))
        (test (interpret #(1 if 2 if 3 endif 4 endif 5) '())
              (5 4 3))
        (test (interpret #(1 if 0 if 2 endif 3 endif 4) '())
              (4 3))))

(define feature-while-loop
  (list (test (interpret #(while wend) '(3 7 4 0 5 9))
              (5 9))
        (test (interpret #(define sum
                            dup
                            while + swap dup wend
                            drop
                            end
                            1 2 3 0 4 5 6 sum)
                         '())
              (15 3 2 1))
        (test (interpret #(define power2
                            1 swap dup
                            while
                                swap 2 * swap 1 - dup
                            wend
                            drop
                            end
                            5 power2 3 power2 power2) '())
              (256 32))))

(define feature-repeat-loop
  (list (test 'TODO TODO)))

(define feature-for-loop
  (list (test (interpret #(define fact
                            1 1 rot for i * next
                            end
                            6 fact 10 fact)
                         '())
              (3628800 720))))
(define feature-global
  (list (test (interpret #(defvar counter 0
                            define next1
                              counter 1 + set counter
                            end
                            counter
                            next1 counter next1
                            counter next1 counter next1 +
                            next1 next1
                            counter next1 counter next1 *)
                         '())
              (42 5 1 0))))
              

(run-tests tests)
(run-tests feature-if-else)
(run-tests feature-nested-if)
(run-tests feature-while-loop)
(run-tests feature-repeat-loop)
(run-tests feature-for-loop)
(run-tests feature-global)
