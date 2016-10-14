#lang racket

(require sicp)

;; (print-as-expression #f)
;; (print-mpair-curly-braces #f)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (cube x)
  (* x x x))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; Exercise 1.29
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (next x) (+ x 1))
  (define (term x)
    (cond ((or (= x 0) (= x n)) (y x))
          ((even? x) (* 2 (y x)))
          (else (* 4 (y x)))))
  (* (/ h 3.0) (sum term 0 next n)))

;; Exercise 1.30
(define (sum/iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Exercise 1.31
;; (a)
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (+ a 1) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-product a b)
  (define (term x)
    (if (even? x)
        (/ (+ x 2.0) (+ x 1.0))
        (/ (+ x 1.0) (+ x 2.0))))
  (product term a inc b))

;; (b)
(define (product/iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;; Exercise 1.32
;; (a)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum/v2 term a next b)
  (accumulate + 0 term a next b))

(define (product/v2 term a next b)
  (accumulate * 1 term a next b))

;; (b)
(define (accumulate/iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Exercise 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (cond ((> a b) null-value)
        ((filter a)
         (combiner (term a)
                   (filtered-accumulate combiner null-value term
                                        (next a) next b filter)))
        (else (filtered-accumulate combiner null-value term
                                   (next a) next b filter))))
;; (a)
(define (square-sum-of-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))
;; (b)
(define (product-of-relative-primes-with n)
  (define (relative-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate * 1 identity 2 inc n relative-prime?))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (average a b)
  (/ (+ a b) 2.0))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;; Exercise 1.35
(define (gold-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

;; Exercise 1.36
(define (fixed-point-with-display f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Exercise 1.37
;; (a)
(define (cont-frac n d k)
  (define (cont-frac-rec i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-frac-rec (+ i 1))))))
  (cont-frac-rec 1))
;; (b)
(define (cont-frac/iter n d k)
  (define (cont-frac-iter i result)
    (if (= i 0)
        result
        (cont-frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (cont-frac-iter k 0))

;; Exercise 1.38
(define (approx-e times)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (i)
                    (if (= (remainder (+ i 1) 3) 0)
                        (* 2 (/ (+ i 1) 3))
                        1))
                  times)))

;; Exercise 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1) x (- (square x))))
             (lambda (i) (- (* 2.0 i) 1.0))
             k))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; Exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

;; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

;; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; Exercise 1.43
(define (repeated f times)
  (lambda (x)
    (if (= times 1)
        (f x)
        ((compose f (repeated f (- times 1))) x))))

;; Exercise 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth f n)
  (lambda (x)
    (((repeated smooth n) f)x)))

;; Exercise 1.45
(define (nth-root x nth)
  (define (nth-root-with-dump x nth dump-cnt)
    (fixed-point ((repeated average-damp dump-cnt)
                  (lambda (y) (/ x (expt y (- nth 1)))))
                 1.0))
  (nth-root-with-dump x nth (floor (log2 nth))))

(define (log2 x)
  (/ (log x) (log 2)))

;; Exercise 1.46
(define (iterative-improve good-enough? improve)
  (define (rec x)
    (let ((next (improve x)))
      (if (good-enough? x next)
          next
          (rec next))))
  rec)

(define (sqrt/ii x)
  ((iterative-improve (lambda (a b) (< (abs (- a b)) 0.001))
                      (average-damp (lambda (y) (/ x y))))
   x))

(define (fixed-point/ii f first-guess)
  ((iterative-improve (lambda (a b) (< (abs (- a b)) 0.001))
                      f)
   first-guess))
