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
  (cont-frac 
    (lambda (i) 
      (if (= 1 i)
        x
        (- (* x x))))
    (lambda (i) 
      (- 1 (* 2 i)))
    k))

