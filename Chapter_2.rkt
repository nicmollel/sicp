#lang racket

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

;Ex. 2.22 
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

