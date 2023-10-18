#lang sicp

(define (cont-frac-recursive N D k)
  (define (recur i)
    (if (< i k)
        (/ (N i)
           (+ (D i) (recur (inc i))))
        (/ (N i)
           (D i))))
  (recur 1))

(define (cont-frac N D k)
  (define (iter result i)
    (if (< i k)
        (iter (/ (N (- k i))
                 (+ (D (- k i)) result))
              (inc i))
        result))
  (iter 0 0))

(define (phi k)
  (cont-frac
    (lambda (i) 1.0)
    (lambda (i) 1.0)
    k))