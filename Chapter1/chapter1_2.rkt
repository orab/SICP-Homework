#lang racket

(require sicp)

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial/iter n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* product counter)
                 (+ counter 1)
                 max-count)))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib/iter n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;; Example: Counting Change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; Exercise 1.11
(define (f/rec n)
  (if (< n 3)
      n
      (+ (f/rec (- n 1))
         (* 2 (f/rec (- n 2)))
         (* 3 (f/rec (- n 3))))))

(define (f/iter n)
  (f-iter 0 1 2 n))

(define (f-iter a b c n)
  (cond ((< n 0) n)
        ((= n 0) a)
        ((= n 1) b)
        ((= n 2) c)
        (else (f-iter b c (+ (* 3 a) (* 2 b) c) (- n 1)))))

;; Exercise 1.12
(define (pascal-trangle-ele row col)
  (cond ((< row 0) 0)
        ((and (< col 0) (> col (- row 1))) 0)
        ((and (= col 0) (= col (- row 1))) 1)
        (else (+ (pascal-trangle-ele (- row 1) (- col 1))
                 (pascal-trangle-ele (- row 1) col)))))

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt/iter b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

;; Exercise 1.16
(define (fast-expt/iter b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b counter product)
  (cond ((= counter 0) product)
        ((even? counter) (fast-expt-iter (square b) (/ counter 2) product))
        (else (fast-expt-iter b (- counter 1) (* product b)))))

;; Exercise 1.17
(define (fast-* a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-* (double a) (halve b)))
        (else (+ a (fast-* a (- b 1))))))

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

;; Exercise 1.18
(define (fast-*/iter a b)
  (fast-*-iter a b 0))

(define (fast-*-iter a counter sum)
  (cond ((= counter 0) sum)
        ((even? counter) (fast-*-iter (double a) (halve counter) sum))
        (else (fast-*-iter a (- counter 1) (+ sum a)))))

;; Exercise 1.19
(define (fast-fib n)
  (fast-fib-iter 1 0 0 1 n))

(define (fast-fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fast-fib-iter a
                        b
                        (+ (square p) (square q))
                        (+ (* 2 p q) (square q))
                        (/ count 2)))
        (else (fast-fib-iter (+ (* b q) (* a q) (* a p))
                             (+ (* b p) (* a q))
                             p
                             q
                             (- count 1)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Example: Testing for Primality
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; Exercise 1.22
(define (search-for-primes start num)
  (cond ((= num 0) (newline))
        ((prime? start)
         (timed-prime-test start)
         (search-for-primes (+ start 1) (- num 1)))
        (else (search-for-primes (+ start 1) num))))

;; Exercise 1.23
(define (smallest-divisor/v2 n)
  (find-divisor n 2))

(define (find-divisor/v2 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor/v2 n (next test-divisor)))))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (prime?/v2 n)
  (= n (smallest-divisor/v2 n)))

(define (timed-prime-test/v2 n)
  (newline)
  (display n)
  (start-prime-test/v2 n (runtime)))

(define (start-prime-test/v2 n start-time)
  (if (prime?/v2 n)
      (report-prime (- (runtime) start-time))))

(define (search-for-primes/v2 start num)
  (cond ((= num 0) (newline))
        ((prime?/v2 start)
         (timed-prime-test/v2 start)
         (search-for-primes/v2 (+ start 1) (- num 1)))
        (else (search-for-primes/v2 (+ start 1) num))))

;; Exercise 1.24
(define (timed-prime-test/v3 n)
  (newline)
  (display n)
  (start-prime-test/v3 n (runtime)))

(define (start-prime-test/v3 n start-time)
  (if (fast-prime? n 3)
      (report-prime (- (runtime) start-time))))

(define (search-for-primes/v3 start num)
  (cond ((= num 0) (newline))
        ((fast-prime? start 3)
         (timed-prime-test/v3 start)
         (search-for-primes/v3 (+ start 1) (- num 1)))
        (else (search-for-primes/v3 (+ start 1) num))))

;; Exercise 1.27
(define (fermat-verify n)
  (define (verify a)
    (cond ((<= a 2) true)
          ((= (expmod a n n) a) (verify (- a 1)))
          (else false)))
  (verify (- n 1)))

;; Exercise 1.28
(define (mr-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (mr-square (expmod base (/ exp 2) m) m)
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (mr-square base n)
  (if (and (not (or (= base 1) (= base (- n 1))))
           (= (remainder (square base) n) 1))
      0
      (square base)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (mr-expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime?/v2 n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime?/v2 n (- times 1)))
        (else false)))

