#lang sicp

(#%require (only racket/base
                 error))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; Exercise 2.54
(define (equal?/o a b)
  (if (or (null? a) (not (pair? a)))
      (eq? a b)
      (and (equal?/o (car a) (car b))
           (equal?/o (cdr a) (cdr b)))))
;; end 2.54

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (=number? x n)
  (and (number? x) (= x n)))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

;; Exercise 2.56
(define (deriv/v2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product
                        (make-exponentiation var
                                             (- (exponent exp) 1))
                        (deriv (base exp) var))))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))
;; end 2.56

;; Exercise 2.57
(define (deriv/v3 exp var)

  (define (addend s) (cadr s))

  (define (augend s)
    (if (null? (cdddr s))
        (caddr s)
        (cons '+ (cddr s))))

  (define (multiplier p) (cadr p))

  (define (multiplicand p)
    (if (null? (cdddr p))
        (caddr p)
        (cons '* (cddr p))))

  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv/v3 (addend exp) var)
                              (deriv/v3 (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv/v3 (multiplicand exp) var))
                   (make-product (deriv/v3 (multiplier exp) var)
                                 (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))
;; end 2.57

;; Exercise 2.58
;; a)
(define (deriv/v4 exp var)

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list a1 + a2))))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list m1 '* m2))))

  (define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

  (define (addend s) (car s))

  (define (augend s) (caddr s))

  (define (product? x) (and (pair? x) (eq? (cadr x) '*)))

  (define (multiplier p) (car p))

  (define (multiplicand p) (caddr p))

  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv/v4 (addend exp) var)
                              (deriv/v4 (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv/v4 (multiplicand exp) var))
                   (make-product (deriv/v4 (multiplier exp) var)
                                 (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))
;; b)
(define (deriv/v5 exp var)

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list a1 '+ a2))))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list m1 '* m2))))

  (define (sum? x)
    (and (pair? x)
         (or (eq? (cadr x) '+)
             (and (pair? (cdddr x))
                  (sum? (cddr x))))))

  (define (addend s)
    (if (eq? (cadr s) '+)
        (car s)
        (list (car s) (cadr s) (addend (cddr s)))))

  (define (augend s)
    (if (eq? (cadr s) '+)
        (if (null? (cdddr s))
            (caddr s)
            (cddr s))
        (augend (cddr s))))

  (define (product? x)
    (and (pair? x)
         (eq? (cadr x) '*)
         (or (null? (cdddr x))
             (product? (cddr x)))))

  (define (multiplier p) (car p))

  (define (multiplicand p)
    (if (null? (cdddr p))
        (caddr p)
        (cddr p)))

  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv/v5 (addend exp) var)
                              (deriv/v5 (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv/v5 (multiplicand exp) var))
                   (make-product (deriv/v5 (multiplier exp) var)
                                 (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))
;; end 2.58

;; Sets as unordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))
;; end 2.59

;; Exercise 2.60
(define (element-of-set?/v2 x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set?/v2 x (cdr set)))))

(define (adjoin-set/v2 x set)
  (cons x set))

(define (intersection-set/v2 set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set?/v2 (car set1) set2)
         (cons (car set1) (intersection-set/v2 (cdr set1) set2)))
        (else (intersection-set/v2 (cdr set1) set2))))

(define (union-set/v2 set1 set2)
  (append set1 set2))
;; end 2.60

;; Sets as ordered lists
(define (element-of-set?/v3 x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set?/v3 x (cdr set)))))

(define (intersection-set/v3 set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set/v3 (cdr set1)
                                             (cdr set2))))
              ((< x1 x2)
               (intersection-set/v3 (cdr set1) set2))
              ((< x2 x1)
               (intersection-set/v3 set1 (cdr (set2))))))))

;; Exercise 2.61
(define (adjoin-set/v3 x set)
  (if (null? set)
      (cons x set)
      (let ((e (car set)))
        (cond ((= x e) set)
              ((< x e) (cons x set))
              (else (cons e (adjoin-set/v3 x (cdr set))))))))
;; end 2.61

;; Exercise 2.62
(define (union-set/v3 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set/v3 (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set/v3 (cdr set1) set2)))
                 (else
                  (cons x2 (union-set/v3 set1 (cdr set2)))))))))
;; end 2.62

;; Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set?/v4 x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set?/v4 x (left-branch set)))
        ((> x (entry set))
         (element-of-set?/v4 x (right-branch set)))))

(define (adjoin-set/v4 x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set/v4 (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set/v4 (right-branch set))))))

;; Exercise 2.65
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (intersection-set/v4 set1 set2)
  (define (intersection-set-inner ol1 ol2)
    (if (or (null? ol1) (null? ol2))
        '()
        (let ((x1 (car ol1)) (x2 (car ol2)))
          (cond ((= x1 x2)
                 (cons x1 (intersection-set-inner (cdr ol1) (cdr ol2))))
                ((< x1 x2)
                 (intersection-set-inner (cdr ol1) ol2))
                ((> x1 x2)
                 (intersection-set-inner ol1 (cdr ol2)))))))
  (list->tree (intersection-set-inner (tree->list-1 set1)
                                      (tree->list-1 set2))))

(define (union-set/v4 set1 set2)
  (define (union-set-inner ol1 ol2)
    (cond ((null? ol1) ol2)
          ((null? ol2) ol1)
          (else
           (let ((x1 (car ol1)) (x2 (car ol2)))
             (cond ((= x1 x2)
                    (cons x1 (union-set-inner (cdr ol1) (cdr ol2))))
                   ((< x1 x2)
                    (cons x1 (union-set-inner (cdr ol1) ol2)))
                   ((> x1 x2)
                    (cons x2 (union-set-inner ol1 (cdr ol2)))))))))
  (list->tree (union-set-inner (tree->list-1 set1)
                               (tree->list-1 set2))))
;; end 2.65

;; Exercise 2.66
;; (define (lookup given-key set-of-records)
;;   (cond ((null? set-of-records) false)
;;         ((= given-key (key (entry set-of-records)))
;;          (entry set-of-records))
;;         ((< given-key (key (entry set-of-records)))
;;          (lookup given-key (left-branch set-of-records)))
;;         ((> given-key (key (entry set-of-records)))
;;          (lookup given-key (right-branch set-of-records)))))
;; end 2.66

;; Huffman Encoding Trees
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (symbol-weight x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (code-tree-left-branch tree) (car tree))
(define (code-tree-right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (symbol-weight tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (code-tree-left-branch branch))
        ((= bit 1) (code-tree-right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (code-tree-adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (code-tree-adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (code-tree-adjoin-set (make-leaf (car pair)
                                         (cadr pair))
                              (make-leaf-set (cdr pairs))))))

;; Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((null? tree) (error "tree is empty: ENCODE-SYMBOL" tree))
        ((leaf? tree) '())
        ((element-of-set? symbol (symbols (code-tree-left-branch tree)))
         (cons 0 (encode-symbol symbol (code-tree-left-branch tree))))
        ((element-of-set? symbol (symbols (code-tree-right-branch tree)))
         (cons 1 (encode-symbol symbol (code-tree-right-branch tree))))
        (else (error "symbol is not defined: ENCODE-SYMBOL" symbol))))

;; end 2.68

;; Exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
 (cond ((null? set) '())
        ((null? (cdr set)) (car set))
        (else
         (let ((left (car set)) (right (cadr set)))
           (successive-merge
            (code-tree-adjoin-set (make-code-tree left right)
                                  (cddr set)))))))
;; end 2.69
