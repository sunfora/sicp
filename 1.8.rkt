#lang sicp
(define (square x) (* x x))

(define (average x y) 
  (/ (+ x y) 2))

(define (within x y eps)
  (< (abs (- x y)) eps))

(define (cbrt-iter guess new-guess x)
  (if (good-enough? guess new-guess)
      guess
      (cbrt-iter new-guess (improve new-guess x) x)))

(define (improve y x)
  (/ (+ (/ x (square y)) (* 2.0 y)) 3.0))

(define (good-enough? guess new-guess)
  (within guess new-guess 0.001))

(define (cbrt x)
  (cbrt-iter x (improve x x) x))
