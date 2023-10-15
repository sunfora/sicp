#lang sicp

(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (filtered-combiner a r)
    (if (predicate a) 
      (combiner (term a) r) 
      r))
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (filtered-combiner a result))))
  (iter a null-value))

(define (square x) (* x x))

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))

  (define (find-divisor n test-divisor)
    (define (next x)
      (if (= 2 x) 3 (+ 2 x)))
    (cond ((> (square test-divisor) n) 
           n)
          ((divides? test-divisor n) 
           test-divisor)
          (else (find-divisor 
                 n 
                 (next test-divisor)))))

  (define (divides? a b)
    (= (remainder b a) 0))

  (and (not (= 1 n))
       (= n (smallest-divisor n))))

(define (sum-of-squares-prime a b)
  (filtered-accumulate prime? + 0 square a inc b))

(define (product-gcd n)
  (define (coprime? x) 
    (= 1 (gcd x n)))
  (filtered-accumulate coprime? * 1 identity 1 inc n))

