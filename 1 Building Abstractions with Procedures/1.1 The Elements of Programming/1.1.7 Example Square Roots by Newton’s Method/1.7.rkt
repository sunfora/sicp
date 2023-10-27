#lang sicp

(define (square x) (* x x))

(define (relative-error x y)
  (/ (abs (- x y))
     y))

(define (good-enough? previous-guess guess)
  (< (relative-error previous-guess guess) 0.001))

(define (sqrt-iter previous-guess guess x)
  (if (good-enough? previous-guess guess)
      previous-guess
      (sqrt-iter guess (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 (improve 1.0 x) x))
