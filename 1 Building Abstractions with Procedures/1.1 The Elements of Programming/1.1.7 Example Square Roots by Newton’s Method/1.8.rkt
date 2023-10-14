#lang sicp

(define (square x) (* x x))

(define (cbrt x)
  (define (within x y eps)
    (< (abs (- x y)) eps))

  (define (cbrt-iter guess new-guess)
    (if (good-enough? guess new-guess)
        guess
        (cbrt-iter new-guess (improve new-guess))))

  (define (good-enough? guess new-guess)
    (within guess new-guess 0.001))

  (define (improve y)
    (/ (+ (/ x (square y)) (* 2.0 y)) 3.0))
  
  (cbrt-iter x (improve x)))
