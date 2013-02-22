#lang racket
;; From SICP

(provide prime?)

;not so efficient implementation of test for primality
;Sec 1.2.6

(define (prime? n)
  (= n (smallest-divisor n)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (sqr test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor n)))))

(define (smallest-divisor n)
  (find-divisor n 2))

;;Ex. 1.10
(define (A x y)
  "Ackermann's function"
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;;> (A 1 10)
;;1024
;;> (A 2 4)
;;65536
;;> (A 3 3)
;;65536

(define (f n)
  ;;2n
  (A 0 n))

(define (g n)
  ;;2^{n} for n > 0
  (A 1 n))

(define (h n)
  (A 2 n))

;1.3
(define (sum term a next b)
  "Sum of a function over range [a,b]"
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;Ex. 1.29 
;f(a) + 4f(a+h) + 2f(a+2h) + 4(a+3h)....+f(a+nh)
; 0      1          2          3           n
(define (simpson-intergral f a b n)
  "function intergral by Simpson Rule"
  (define (h)
    (/(- b a) n))
  (define (add-h  x)
    (+ x (h)))
  (define (sum x yk)
    (let ((yn (f yk)))
      (if (> x n)
          0 
          (+ (cond ((or (= x 0) (= x n)) yn)
                 ((even? x) (* 2 yn))
                 ((* 4 yn)))
             (sum (+ x 1) (add-h yk))))))
  (* (/ (h) 3.0)
     (sum 0 a))) 


;Ex. 1.42
;x -> f(g(x))
(define (my-compose f g)
  (lambda (x)(f (g x))))