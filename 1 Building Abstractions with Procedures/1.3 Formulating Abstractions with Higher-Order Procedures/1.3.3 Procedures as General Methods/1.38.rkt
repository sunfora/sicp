#lang sicp

(define (cont-frac N D k)
  (define (iter result i)
    (if (< i k)
        (iter (/ (N (- k i))
                 (+ (D (- k i)) result))
              (inc i))
        result))
  (iter 0 0))

(define (e k) (+ 2.0 (cont-frac
                    (lambda (i) 1)
                    (lambda (i) 
                           (if (= 2 (remainder i 3))
                               (* 2 (/ (+ i 1) 3))
                               1))
                         k)))