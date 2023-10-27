#lang sicp

(define (square x) (* x x))

(define (relative-error x y)
  (/ (abs (- x y))
     y))

(define (good-enough? previous-guess guess)
  (< (relative-error previous-guess guess) 0.001))

(define (cbrt-iter previous-guess guess x)
  (if (good-enough? previous-guess guess)
      previous-guess
      (cbrt-iter guess (improve guess x) x)))

(define (improve y x)
  (/ (+ (/ x (square y)) (* 2.0 y)) 3.0))

(define (average x y)
  (/ (+ x y) 2))

(define (cbrt x)
  (cbrt-iter 1.0 (improve 1.0 x) x))
