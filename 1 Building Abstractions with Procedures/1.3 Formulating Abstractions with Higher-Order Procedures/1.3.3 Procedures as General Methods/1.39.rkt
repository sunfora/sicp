#lang sicp

(define (cont-frac N D k)
  (define (iter result i)
    (if (< i k)
        (iter (/ (N (- k i))
                 (+ (D (- k i)) result))
              (inc i))
        result))
  (iter 0 0))

(define (tan-cf x k)
  (exact->inexact (cont-frac 
    (lambda (i) 
      (if (= 1 i)
        x
        (- (* x x))))
    (lambda (i) 
      (dec (* 2 i)))
    k)))

