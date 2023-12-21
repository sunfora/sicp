#lang sicp

(define (expt* x p)
  (if (= p 0)
      1
      (* x (expt* x (dec p)))))

(define (divides? x y)
  (= 0 (remainder x y)))

(define (div-times x y)
  (if (divides? x y)
    (inc (div-times (/ x y) y))
    0))

(define (cons x y)
  (* (expt* 2 x) (expt* 3 y)))

(define (car x)
  (div-times x 2))

(define (cdr x)
  (div-times x 3))
