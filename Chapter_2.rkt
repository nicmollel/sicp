#lang racket
;use stuff from Chapter 1
(require "Chapter_1.rkt")

;provide some functions for later Chapters
(provide gcd
         )

;Chapter 2
;GCD
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (average x y)
  (/ (+ x y) 2.0))

;Ex. 2.1 
(define (make-rat n d)
  "Make a Rational Number representation"
  (let ((g (gcd (abs n) (abs d))))
    ;this was the shortest way I could come up to normalize
    ;the arguments 
    (if (> 0 d) (cons (/ (* -1 n) g)
                      (/ (* -1 d) g))
        (cons (/ n g) (/ d g)))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

;i changed this to a single call which is neater in my opinion. 
;not sure if fprintf is portable though! 
(define (print-rat x)
  (fprintf (current-output-port) ;can it handle printing 1?
           "~a/~a~n" (numer x) (denom x)))

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

;One thing that can be done to improve the above implementation is
;adding handling on 1 (1/1)! 

;Ex. 2.2
;line segment in a plane
(define (make-segment start end)
  (cons start end))

(define (start-segment x)
  (car x))

(define (end-segment x)
  (cdr x))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(define (length-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (sqrt (+ (expt (- (x-point end) (x-point start)) 2)
             (expt (- (y-point end) (y-point start)) 2)))))


;Ex. 2.5 
(define (exp-cons a b)
  ;x=2^{a}3^{b}
  (* (expt 2 a) (expt 3 b)))

;how do you write exp-car and exp-cdr?

;Ex. 2.20
(define (same-parity x . rest)
  "return all elements in rest with the same odd/even parity as x"
  (if (even? x)
      (cons x (filter even? rest))
      (cons x (filter odd? rest))))
;at the time of the exercise, a lot of functions have not been
;introduced so the expected implementation of the above should 
;probably use recursion with a function

;something along these lines
(define (same-parity-long x . rest)
  (define (check-parity proc list)
    (cond ((null? list) list)
          ((proc (car list)) (cons (car list) (check-parity proc (cdr list))))
          (else (check-parity proc (cdr list)))))
  (if (even? x)
      (cons x (check-parity even? rest))
      (cons x (check-parity odd? rest))))
;could this be rewritten with recursion but without the helper procedure?


;Ex. 2.21
(define (square-list items)
  (if (null? items)
      items
      (cons (sqr (car items))(square-list (cdr items)))))

(define (square-list-map items)
  (map sqr items))

;Ex. 2.23
(define (for-each proc items)
  "apply proc to each item of the list"
  (cond ((null? items) #t)
        (else (proc (car items))
              (for-each proc (cdr items)))))


;Ex. 2.27 
;normal reverse 
(define (my-reverse items)
  (if (null? items)
      items
      (append (my-reverse (cdr items)) (list (car items)))))

;deep reverse
;I don't like how it complains on (car/cdr '()) as these should be NIL
(define (deep-reverse items)
  (cond ((null? items) items)
        ((pair? (car items))
         (append (deep-reverse (cdr items)) (list (reverse (car items)))))
        (else 
         (append (deep-reverse (cdr items)) (list (car items))))))

;Ex. 2.28
(define (fringe items)
  (cond ((null? items) items)
        ((pair? (car items))
         (append (fringe (car items)) (fringe (cdr items))))
        (else 
         (append (list(car items)) (fringe (cdr items))))))

;deep-reverse and fringe are really the same procedure just differ
;in the order or processing and the result they return

(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

;Ex. 2.30
(define (square-tree tree)
  "Square leaves of a tree"
  (cond ((null? tree) tree)
        ((not (pair? tree)) (sqr tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  "Square tree maps using map"
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (sqr sub-tree)))
       tree))

;Ex. 2.31
;as above, this can be with recursion or using map. since the name 
;is tree map, i am gonna use map
(define (tree-map proc tree)
  "apply proc to the leave nodes of tree"
  (map (lambda (sub-tree)
         (if (pair? sub-tree) 
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

;Ex. 2.32
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))rest)))))

;Sect 2.2.3
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;;; the built in function like this is foldr
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;Ex. 2.33
(define (accumulate-map p seq)
  (accumulate (lambda (x y)(cons (p x) y)) null seq))

(define(accumulate-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (accumulate-length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

;Ex. 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;Nested Mappings

;accumulating pairs
((lambda (n)
   (accumulate append null 
               (map (lambda (i)
                      (map (lambda (j) (list i j))
                           (enumerate-interval 1 (- i 1))))
                    (enumerate-interval 1 n)))) 6)

;abstrating mapping and accumulating with append
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

;same procedure as above becomes
((lambda (n)
   (flatmap (lambda (i)
              (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n))) 6)

(define (prime-sum? pair)
  (prime? (+ (car pair)(cadr pair))))

(define (make-pair-sum pair)
  (let ((x (car pair))
        (y (cadr pair)))
    (list x y (+ x y))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? 
               (flatmap (lambda(i)
                          (map (lambda(j)(list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

;permuations  of a set presented as a list
(define (permutations s)
  (if (null? s)
      (list s)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

;Ex. 2.40
;i think unique-pairs has really been defined just that it not use
;flatmap....and i went ahead and did so above. so below is really
;a copy of the the anonymous function above with a name
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;prime-sum-pairs can now be rewritten as 
(define (primesum-pairs n)
  (map make-pair-sum 
       (filter prime-sum? (unique-pairs n))))

;Ex. 2.41
;change the filter function from prime-sum? to one that checks if
;the pair adds to a given sum
(define (sum-s-pairs n s)
  (map make-pair-sum 
       (filter (lambda(p)
                 (= s (+ (car p) (cadr p))))
               (unique-pairs n))))

;Ex 2.42
;I don't remember the solution but the 'eight queen' prob gave me some
;hard time in my intro classes

(define empty-board null)

;board positions => (row,column)
(define (adjoin-position row col rest-of-queens)
  (cons (list row col)  rest-of-queens))

(define (safe? kth-col positions)
  ;positions is a list of (new-quenn-pos valid-queen-pos)
  ;if row or valid is the same as new, it's not safe
  ;col should technically be different 
  ;if new is in the same diagonal as valid, then it's not safe

  ;my solutions was not geting me anywhere so i checked the solution
  ;for safe? from scheme wiki
  (define (safe-row?)
    (null? (filter (lambda(pos)
                     (= (car pos)(caar positions)))
                   (cdr positions))))
  (define (safe-diag?)
    (null? (filter (lambda(pos)
                     (= (abs (- (caar positions)(car pos)))
                        (abs (- (cadar positions)(cadr pos)))))
                   (cdr positions))))
  ;this is a neat trick that the row,col difference for diagonals
  ;is the same! 
  (and (safe-row?)(safe-diag?)))

;solution with the gaps
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board) 
        (filter (lambda(positions)
                  (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

;2.3 Symbolic Data

;Ex. 2.54
;the simple version here work for lists but fails for atoms
(define (my-equal? x y)
  ;this version is suppose to handle numbers better
  (cond ((and (number? x)(number? y)) (= x y))
        ((or (not (pair? x))(not (pair? y)))  (eq? x y))
         ((and (null? x)(null? y)) #t)
            ((eq? (car x)(car y))(my-equal? (cdr x)(cdr y)))
            (else #f)))

;symbolic differentiation
(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1)(variable? v2)(eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp)(= exp num)))

;Ex. 2.57
;;; Extend the sum and product constructors to handle arbitrary
;;; number of terms. Really two or more terms

;; the implementation gets the job done but does a terrible job at
;; simplifying the calculations

;this idea was lifted off stack overflow
(define (make-sum . terms)
  (define (sum-terms sum tlist)
    (cond ((null? tlist)(if (= 0 sum) tlist (list sum)))
          ((number? (car tlist)) (sum-terms (+ sum (car tlist))
                                           (cdr tlist)))
          (else(if (null? (car tlist))
                   (sum-terms sum (cdr tlist))
                   (cons (car tlist)(sum-terms sum (cdr tlist))))))) 
  (let ((results (sum-terms 0 terms)))
    (if (= (length results) 1)
        (car results)
        (cons '+ results))))

; would be nice to write the innerds of these two procedures 
; with one macro and half their size space
(define (make-product . terms)
  (define (multiply-terms product tlist)
    (cond ((null? tlist) (if (= 1 product) tlist (list product)))
          ((=number? (car tlist) 0) (list 0))
          ((number? (car tlist))(multiply-terms (* (car tlist))
                                                (cdr tlist)))
          (else (if (null? (car tlist))
                    (multiply-terms product (cdr tlist))
                    (cons (car tlist)(multiply-terms product 
                                                     (cdr tlist)))))))
  (let ((results (multiply-terms 1 terms)))
    (if (= 1 (length results))
        (car results)
        (cons '* results))))

(define (sum? expr)
  (and (pair? expr)(eq? '+ (car expr))))

(define (addend s)
  (cadr s))

(define (augend s)
  (if (> (length s) 3)      
      (cons '+ (cddr s))
      (caddr s)))

(define (product? x)
  (and (pair? x)(eq? '* (car x))))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (if (> (length p) 3)
      (cons '* (cddr p))
      (caddr p)))

;Ex. 2.56
;;; extend (deriv...) to be able to deferentiate exponention 
;;; expressions

;;; helper function
;;; I will define n^{x} as (^ n x)

(define (exponentiation? expr)
  (and (pair? expr)(eq? '^ (car expr))))

(define (make-exponentiation b e)
  (cond ((or (=number? e 0)(=number? b 1)) 1)
        ((=number? b 0) 0)
        ((=number? e 1) b)
        ((and (number? b)(number? e))(expt b e))
        (else (list '^ b e))))

(define (base expr)
  (cadr expr))

(define (exponent expr)
  (caddr expr))

; derivative 
(define (deriv expr var)
  "Derivative of expr with respect to var"
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum 
          (make-product (multiplier expr)
                        (deriv (multiplicand expr) var))
          (make-product (multiplicand expr)
                        (deriv (multiplier expr) var))))
        ;;Ex. 2.56 work
        ((exponentiation? expr)
         (make-product
          (make-product (exponent expr)
                        (make-exponentiation (base expr)
                                             (make-sum (exponent expr)
                                                       -1)))
          (deriv (base expr) var)))
        (else 
         (error "unknown expression type -- DERIV" expr))))

