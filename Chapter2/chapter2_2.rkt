#lang sicp

(#%require sicp-pict)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length/iter items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; Exercise 2.17
(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))
;; end 2.17

;; Exercise 2.18
(define (reverse list)
  (define (reverse-iter list result)
    (if (null? list)
        result
        (reverse-iter (cdr list) (cons (car list) result))))
  (reverse-iter list nil))
;; end 2.18

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;; Exercise 2.19
(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))
;; end 2.19

;; Exercise 2.20
(define (same-parity . lst)
  (define (pick x remains)
    (cond ((null? remains) nil)
          ((= (remainder (- (car remains) x) 2) 0)
           (cons (car remains) (pick x (cdr remains))))
          (else
           (pick x (cdr remains)))))
  (pick (car lst) lst))
;; end 2.20

(define (map/v3 proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map/v3 proc (cdr items)))))

(define (square x)
  (* x x))

;; Exercise 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list/v2 items)
  (map square items))
;; end 2.21

;; Exercise 2.23
(define (for-each proc items)
  (if (null? items)
      nil
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))
;; end 2.23

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Exercise 2.27
(define (deep-reverse list)
  (define (reverse-iter list result)
    (cond ((null? list) result)
          ((not (pair? list)) list)
          (else
           (reverse-iter (cdr list)
                         (cons (reverse-iter (car list) nil) result)))))
  (reverse-iter list nil))
;; end 2.27

;; Exercise 2.28
(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x))(list x))
        (else (append (fringe (car x)) (fringe (cdr x))))))
;; end 2.28

;; Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a)
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; b)
(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

;; c)
(define (balanced? mobile)
  (if (or (null? mobile)
          (not (pair? mobile)))
      true
      (and (= (* (branch-length (left-branch mobile))
                 (total-weight (branch-structure (left-branch mobile))))
              (* (branch-length (right-branch mobile))
                 (total-weight (branch-structure (right-branch mobile)))))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

;; d)
(define (make-mobile/v2 left right)
  (cons left right))

(define (make-branch/v2 length structure)
  (cons length structure))

(define (left-branch/v2 mobile)
  (car mobile))

(define (right-branch/v2 mobile)
  (cdr mobile))

(define (branch-length/v2 branch)
  (car branch))

(define (branch-structure/v2 branch)
  (cdr branch))
;; end 2.29

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree/v2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;; Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree/v2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree/v2 sub-tree)
             (square sub-tree)))
       tree))
;; end 2.30

;; Exercise 2.31
(define (square-tree/v3 tree) (tree-map square tree))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
;; end 2.31

;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (lst) (cons (car s) lst)) rest)))))
;; end 2.32

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (fib n)
  (define (fib-iter a b counter)
    (if (<= n counter)
        b
        (fib-iter b (+ a b) (+ counter 1))))
  (fib-iter 1 0 0))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumarate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumarate-tree (car tree))
                      (enumarate-tree (cdr tree))))))

(define (sum-odd-squares/v2 tree)
  (accumulate + 0 (map square (filter odd? (enumarate-tree tree)))))

(define (even-fibs/v2 n)
  (accumulate cons nil (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons nil (map square (map fib (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

;; Exercise 2.33
(define (map/v2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append/v2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length/v2 sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
;; end 2.33

;; Exercise 2.34
(define (horner-eval x coefficient-sequece)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequece))
;; end 2.34

;; Exercise 2.35
(define (count-leaves/v2 t)
  (accumulate + 0 (map (lambda (x) 1) (enumarate-tree t))))
;; end 2.35

;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
;; end 2.36

;; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector m v)) cols)))
;; end 2.37

;; Exercise 2.38
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
;; end 2.38

;; Exercise 2.39
(define (reverse/v2 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse/v3 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
;; end 2.39

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

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (x)
             (map (lambda (y) (list x y))
                  (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs/v2 n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
;; end 2.40

;; Exercise 2.41
(define (triple-sum-less-than-s n s)
  (define (make-triple-sum triple)
    (list (car triple) (cadr triple) (caddr triple)
          (+ (car triple) (cadr triple) (caddr triple))))
  (define (sum-less-than-s? triple)
    (< (+ (car triple) (cadr triple) (caddr triple)) s))
  (define (unique-triples n)
    (flatmap (lambda (x)
               (flatmap (lambda (z)
                          (map (lambda (y) (list x y z))
                               (enumerate-interval (+ z 1) (- x 1))))
                        (enumerate-interval 1 (- x 1))))
             (enumerate-interval 1 n)))
  ;; main
  (map make-triple-sum
       (filter sum-less-than-s?
               (unique-triples n))))
;; end 2.41

;; Exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (define empty-board '())
  (define (adjoin-position new-row k rest-of-queens)
    (cons (list new-row k) rest-of-queens))
  (define (safe? k positions)
    (let ((positions-with-k
           (filter (lambda (pos) (= k (cadr pos))) positions))
          (positions-other
           (filter (lambda (pos) (not (= k (cadr pos)))) positions)))
      (if (not (null? (cdr positions-with-k)))
          false
          (let ((pos-k (car positions-with-k)))
            (accumulate (lambda (x y) (and x y))
                        true
                        (map (lambda (pos-other)
                               (let ((xk (car pos-k)) (yk (cadr pos-k))
                                     (xo (car pos-other)) (yo (cadr pos-other)))
                                 (and (not (= xk xo))
                                      (not (= (abs (- xk xo))
                                              (abs (- yk yo)))))))
                             positions-other))))))
  ;; main
  (queen-cols board-size))
;; end 2.42

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
;; end 2.44

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs/v2 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit/v2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;; Exercise 2.45
(define (split half1 half2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split half1 half2) painter (- n 1))))
          (half1 painter (half2 smaller smaller))))))
;; end 2.45

(define (frame-coord-map/o frame)
  (lambda (v)
    (add-vect/o
     (origin-frame/v1 frame)
     (add-vect/o (scale-vect/o (xcor-vect/o v) (edge1-frame/v1 frame))
                 (scale-vect/o (ycor-vect/o v) (edge2-frame/v1 frame))))))

;; Exercise 2.46
(define (make-vect/o xcor ycor)
  (cons xcor ycor))

(define (xcor-vect/o vect)
  (car vect))

(define (ycor-vect/o vect)
  (cdr vect))

(define (add-vect/o v1 v2)
  (make-vect/o (+ (xcor-vect/o v1) (xcor-vect/o v2))
               (+ (ycor-vect/o v1) (ycor-vect/o v2))))

(define (sub-vect/o v1 v2)
  (make-vect/o (- (xcor-vect/o v1) (xcor-vect/o v2))
               (- (ycor-vect/o v1) (ycor-vect/o v2))))

(define (scale-vect/o s vect)
  (make-vect/o (* s (xcor-vect/o vect)) (* s (ycor-vect/o vect))))
;; end 2.46

;; Exercise 2.47
(define (make-frame/v1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame/v1 frame)
  (car frame))

(define (edge1-frame/v1 frame)
  (car (cdr frame)))

(define (edge2-frame/v1 frame)
  (car (cdr (cdr frame))))

(define (make-frame/v2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame/v2 frame)
  (car frame))

(define (edge1-frame/v2 frame)
  (car (cdr frame)))

(define (edge2-frame/v2 frame)
  (cdr (cdr frame)))
;; end 2.47

(define (segments->painter/o segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map/o frame)
         (start-segment segment))
        ((frame-coord-map/o frame)
         (end-segment segment))))
     (segment-list))))

(define (draw-line fragment-list)
  ;; do nothing, only for passing compiling
  '())

;; Exercise 2.48
(define (make-segment/v1 start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))
;; end 2.48

;; Exercise 2.49
;; a)
(define outline-of-frame
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 0 0.99))
         (make-segment (make-vect 0 0.99) (make-vect 0.99 0.99))
         (make-segment (make-vect 0.99 0.99) (make-vect 0.99 0))
         (make-segment (make-vect 0.99 0) (make-vect 0 0)))))
;; b)
(define X-painter
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
         (make-segment (make-vect 0 1) (make-vect 1 0)))))
;; c)
(define diamond
  (segments->painter
   (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
         (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
         (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
         (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))
;; d)
(define wave
  (segments->painter
   (list (make-segment (make-vect 0.006 0.840) (make-vect 0.155 0.591))
         (make-segment (make-vect 0.006 0.635) (make-vect 0.155 0.392))
         (make-segment (make-vect 0.304 0.646) (make-vect 0.155 0.591))
         (make-segment (make-vect 0.298 0.591) (make-vect 0.155 0.392))
         (make-segment (make-vect 0.304 0.646) (make-vect 0.403 0.646))
         (make-segment (make-vect 0.298 0.591) (make-vect 0.354 0.492))
         (make-segment (make-vect 0.403 0.646) (make-vect 0.348 0.845))
         (make-segment (make-vect 0.354 0.492) (make-vect 0.249 0.000))
         (make-segment (make-vect 0.403 0.000) (make-vect 0.502 0.293))
         (make-segment (make-vect 0.502 0.293) (make-vect 0.602 0.000))
         (make-segment (make-vect 0.348 0.845) (make-vect 0.403 0.999))
         (make-segment (make-vect 0.602 0.999) (make-vect 0.652 0.845))
         (make-segment (make-vect 0.652 0.845) (make-vect 0.602 0.646))
         (make-segment (make-vect 0.602 0.646) (make-vect 0.751 0.646))
         (make-segment (make-vect 0.751 0.646) (make-vect 0.999 0.343))
         (make-segment (make-vect 0.751 0.000) (make-vect 0.597 0.442))
         (make-segment (make-vect 0.597 0.442) (make-vect 0.999 0.144)))))
;; end 2.49

(define (transform-painter/o painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (vector-sub (m corner1) new-origin)
                  (vector-sub (m corner2) new-origin)))))))

(define (flip-vert/o painter)
  (transform-painter/o painter
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter/o painter
                       (make-vect 0.5 0.5)
                       (make-vect 1.0 0.5)
                       (make-vect 0.5 1.0)))

(define (rotate90/o painter)
  (transform-painter/o painter
                       (make-vect 1.0 0.0)
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter/o painter
                       (make-vect 0.0 0.0)
                       (make-vect 0.65 0.35)
                       (make-vect 0.35 0.65)))

(define (beside/o painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter/o painter1
                                (make-vect 0.0 0.0)
                                split-point
                                (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter/o painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;; Exercise 2.50
(define (flip-horiz/o painter)
  (transform-painter/o painter
                       (make-vect 1.0 0.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0)))

(define (rotate180/o painter)
  (transform-painter/o painter
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 0.0)))

(define (rotate270/o painter)
  (transform-painter/o painter
                       (make-vect 0.0 1.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0)))
;; end 2.50

;; Exercise 2.51
(define (below/v1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter/o painter1
                                (make-vect 0.0 0.0)
                                (make-vect 1.0 0.0)
                                split-point))
          (paint-top
           (transform-painter/o painter2
                                split-point
                                (make-vect 1.0 0.5)
                                (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below/v2 painter1 painter2)
  (rotate90/o (beside/o (rotate270/o painter1)
                        (rotate270/o painter2))))
;; end 2.51

;; Exercise 2.52
;; a)
(define wave/o
  (segments->painter
   (list (make-segment (make-vect 0.006 0.840) (make-vect 0.155 0.591))
         (make-segment (make-vect 0.006 0.635) (make-vect 0.155 0.392))
         (make-segment (make-vect 0.304 0.646) (make-vect 0.155 0.591))
         (make-segment (make-vect 0.298 0.591) (make-vect 0.155 0.392))
         (make-segment (make-vect 0.304 0.646) (make-vect 0.403 0.646))
         (make-segment (make-vect 0.298 0.591) (make-vect 0.354 0.492))
         (make-segment (make-vect 0.403 0.646) (make-vect 0.348 0.845))
         (make-segment (make-vect 0.354 0.492) (make-vect 0.249 0.000))
         (make-segment (make-vect 0.403 0.000) (make-vect 0.502 0.293))
         (make-segment (make-vect 0.502 0.293) (make-vect 0.602 0.000))
         (make-segment (make-vect 0.348 0.845) (make-vect 0.403 0.999))
         (make-segment (make-vect 0.602 0.999) (make-vect 0.652 0.845))
         (make-segment (make-vect 0.652 0.845) (make-vect 0.602 0.646))
         (make-segment (make-vect 0.602 0.646) (make-vect 0.751 0.646))
         (make-segment (make-vect 0.751 0.646) (make-vect 0.999 0.343))
         (make-segment (make-vect 0.751 0.000) (make-vect 0.597 0.442))
         (make-segment (make-vect 0.597 0.442) (make-vect 0.999 0.144))
         ;; add a smile
         (make-segment (make-vect 0.400 0.850) (make-vect 0.430 0.850))
         (make-segment (make-vect 0.600 0.850) (make-vect 0.570 0.850))
         (make-segment (make-vect 0.450 0.750) (make-vect 0.550 0.750))
         (make-segment (make-vect 0.450 0.750) (make-vect 0.430 0.770))
         (make-segment (make-vect 0.550 0.750) (make-vect 0.570 0.770)))))
;; b)
(define (corner-split/o painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
;; c)
(define (square-limit/v3 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split (flip-horiz/o painter) n))))
;; end 2.52
