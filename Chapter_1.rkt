#lang racket
;; From SICP

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
