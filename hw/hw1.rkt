
;;;;;;;;;;;Task 1

(define (old-day-of-week day month year)
  (remainder (+ day (quotient  (* 31 (+ month (* 12 (quotient  (- 14 month) 12)) -2)) 12) (- year (quotient  (- 14 month) 12))
     (quotient  (- year (quotient  (- 14 month) 12)) 4) (* -1 (quotient  (- year (quotient  (- 14 month) 12)) 100))
     (quotient  (- year (quotient  (- 14 month) 12)) 400)) 7))

(define (day-of-week day month year)
  (let* ((a (quotient  (- 14 month) 12))
         (year (- year a))
         (month (+ month (* 12 a) -2)))
    (remainder (+ day (quotient (* month 31) 12) year (quotient year 4) (- (quotient year 100)) (quotient year 400)) 7)))

;;;;;;;;;;;;Task 2

(define (old-square-roots a b c)
  (if (> (* b b) (* 4 a c))
      (list (/ (- (sqrt (- (* b b) (* 4 a c)) ) b) (* 2 a )) (/ (- 0 (sqrt (- (* b b) (* 4 a c)) ) b) (* 2 a )))
      (if (= (* b b) (* 4 a c))
          (list (/ (* -1 b) (* 2 a)))
          (list))))

(define (square-roots a b c)
  (cond ((> (* b b) (* 4 a c)) (list (/ (- (sqrt (- (* b b) (* 4 a c)) ) b) (* 2 a )) (/ (- 0 (sqrt (- (* b b) (* 4 a c)) ) b) (* 2 a )))) 
        ((= (* b b) (* 4 a c)) (list (/ (* -1 b) (* 2 a))))
        (else (list))))

;;;;;;;;;;;;;Task 3

(define (my-gcd a b)
  (if (not (= b 0))
      (my-gcd b (remainder a b))
      a))
  
(define (my-lcm a b)
  (if (and (= a 0) (= b 0))
      0
      (/ (* a b) (my-gcd a b))))

(define (fac n)
  (if (=  n 1)
      1
      (* n (fac (- n 1)))))

(define (prime? n)
  (= 0 (remainder (+ (fac (- n 1)) 1) n)))


;;;;;;;;;;;;Tests

(display "Task 1:\n")
(display "13 09 2021: ")
(display (day-of-week 13 09 2021))
(display "\n")
(display "14 09 2021: ")
(display (day-of-week 14 09 2021))
(display "\n")
(display "15 09 2021: ")
(display (day-of-week 15 09 2021))
(display "\n")
(display "16 09 2021: ")
(display (day-of-week 16 09 2021))
(display "\n")
(display "17 09 2021: ")
(display (day-of-week 17 09 2021))
(display "\n")
(display "18 09 2021: ")
(display (day-of-week 18 09 2021))
(display "\n")
(display "19 09 2021: ")
(display (day-of-week 19 09 2021))
(display "\n\n")

(display "Task 2:\n")
(display "x^2 + 5x + 6 = 0\n")
(display (square-roots 1 5 6))
(display "\n")
(display "3x^2 - 12x + 12 = 0\n")
(display (square-roots 3 -12 12))
(display "\n")
(display "x^2 - 5x + 7\n")
(display (square-roots 1 -5 7))
(display "\n\n")

(display "Task 3:\n")
(display "gcd of 3542 2464: ")
(display (my-gcd 3542 2464))
(display "\n")
(display "lcm of 3 4: ")
(display (my-lcm 3 4))
(display "\n")
(display "prime? 11: ")
(display (prime? 11))
(display "\n")
(display "prime? 12: ")
(display (prime? 12))
(display "\n")