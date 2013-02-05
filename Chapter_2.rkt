#lang racket

;Chapter 2
;GCD
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

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
  (fprintf (current-output-port)
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

;Ex. 2.2
;line segment in a plane