#lang sicp

(define (square x) (* x x))

(define (sum-of-square x y)
  (+ (square x) (square y)))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

;; Exercise 1.3
(define (sum-of-top2-squares x y z)
  (cond ((and (>= x z) (>= y z)) (sum-of-square x y))
        ((and (>= x y) (>= z y)) (sum-of-square x z))
        (else (sum-of-square y z))))

;; Square Roots by Newton's Method
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exercise 1.7
(define (sqrt-iter/v2 guess last-guess x)
  (if (good-enough?/v2 guess last-guess)
      guess
      (sqrt-iter/v2 (improve guess x)
                    guess
                    x)))

(define (good-enough?/v2 guess last-guess)
  (< (abs (/ (- guess last-guess) guess)) 0.001))

(define (sqrt/v2 x)
  (sqrt-iter/v2 1.0 0 x))

;; Exercise 1.8
(define (cbrt-iter guess last-guess x)
  (if (good-enough?/v2 guess last-guess)
      guess
      (cbrt-iter (cube-improve guess x)
                 guess
                 x)))

(define (cube-improve guess x)
  (average guess (/ (+ (/ x (square guess))
                       (* 2 guess))
                    3)))

(define (cbrt x)
  (cbrt-iter 1.0 0.0 x))
