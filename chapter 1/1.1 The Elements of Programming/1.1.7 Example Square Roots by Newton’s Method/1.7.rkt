#lang sicp
(define (square x) (* x x))

(define (average x y) 
  (/ (+ x y) 2))

(define (within x y eps)
  (< (abs (- x y)) eps))

(define (sqrt x)
  (define (sqrt-iter guess new-guess)
    (if (good-enough? guess new-guess)
        guess
        (sqrt-iter new-guess (improve new-guess))))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (good-enough? guess new-guess)
    (within guess new-guess 0.001))

  (sqrt-iter 1.0 (/ x 2.0)))
