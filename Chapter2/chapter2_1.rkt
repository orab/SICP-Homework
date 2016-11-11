#lang racket

(require sicp)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat/old1 n d) (cons n d))

(define (make-rat/old2 n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; Exercise 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< (* n d) 0)
        (cons (- (abs (/ n g))) (abs (/ d g)))
        (cons (abs (/ n g)) (abs (/ d g))))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Exercise 2.2
(define (midpoint-segment segment)
  (make-segment (/ (+ (x-point (start-segment segment))
                      (x-point (end-segment segment)))
                   2)
                (/ (+ (y-point (start-segment segment))
                      (y-point (end-segment segment)))
                   2)))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

;; Exercise 2.3
(define (make-rect seg-top seg-left)
  (cons seg-top seg-left))

(define (seg-length segment)
  (let ((dx (- (x-point (end-segment segment))
               (x-point (start-segment segment))))
        (dy (- (y-point (end-segment segment))
               (y-point (start-segment segment)))))
    (sqrt (+ (square dx) (square dy)))))

(define (rect-width rect)
  (seg-length (car rect)))


(define (rect-height rect)
  (seg-length (cdr rect)))

(define (square x)
  (* x x))

(define (rect-perimeter rect)
  (*  2 (+ (rect-width rect) (rect-height rect))))

(define (rect-area rect)
  (* (rect-width rect) (rect-height rect)))

(define (make-rect/v2 top-left top-right bottom-left)
  (cons top-left (cons top-right bottom-left)))

(define (rect-width/v2 rect)
  (seg-length (make-segment (car rect) (car (cdr rect)))))

(define (rect-height/v2 rect)
  (seg-length (make-segment (car rect) (cdr (cdr rect)))))

(define (cons/v1 x y)
  (lambda (m) (m x y)))

(define (car/v1 z)
  (z (lambda (p q) p)))

;; Exercise 2.4
(define (cdr/v1 z)
  (z (lambda (p q) q)))

;; Exercise 2.5
(define (cons/v2 a b)
  (* (expt 2 a) (expt 3 b)))

(define (factor-times n c)
  (if (= (remainder n c) 0)
      (+ 1 (factor-times (/ n c) c))
      0))

(define (car/v2 z)
  (factor-times z 2))

(define (cdr/v2 z)
  (factor-times z 3))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; Exercise 2.6
(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

;; Exercise 2.7
(define (upper-bound i)
  (cdr i))

(define (lower-bound i)
  (car i))

;; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.9
(define (spans-zero? interval)
  (< (* (upper-bound interval) (lower-bound interval)) 0))

;; Exercise 2.10
(define (div-interval/v2 x y)
  (if (spans-zero? y)
      (error "it's unexpected when spans zero" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; Exercise 2.11
(define (mul-interval/v2 x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((>= xl 0)
           (cond ((>= yl 0)
                  (make-interval (* xl yl) (* xu yu)))
                 ((and (< yl 0) (>= yu 0))
                  (make-interval (* xu yl) (* xu yu)))
                 (else
                  (make-interval (* xu yl) (* xl yu)))))
          ((and (< xl 0) (>= xu 0))
           (cond ((>= yl 0)
                  (make-interval (* xl yu) (* xu yu)))
                 ((and (< yl 0) (>= yu 0))
                  (make-interval (min (* xl yu) (* xu yl))
                                 (max (* xl yl) (* xu yu))))
                 (else
                  (make-interval (* xu yl) (* xl yl)))))
          (else
           (cond ((>= yl 0)
                  (make-interval (* xl yu) (* xu yl)))
                 ((and (< yl 0) (>= yu 0))
                  (make-interval (* xl yu) (* xl yl)))
                 (else
                  (make-interval (* xu yu) (* xl yl))))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

;; Exercise 2.12
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))

(define (percent i)
  (* 100 (/ (width i) (center i))))
;; end 2.12

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
