#lang sicp


(define (fib n)
  (fib-iter 1 0 0 1 n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; it can be shown that 
; p' = (  p  )^2 + q^2
; q' = (p + q)^2 - p^2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (recompute-q p q)
  (- (square (+ p q)) (square p)))

(define recompute-p sum-of-squares)

(define (fib-iter a b p q count)
  (cond ((= count 0) 
         b)
        ((even? count)
         (fib-iter a
                   b
                   (recompute-p p q)
                   (recompute-q p q)
                   (/ count 2)))
        (else 
         (fib-iter (+ (* b q) 
                      (* a q) 
                      (* a p))
                   (+ (* b p) 
                      (* a q))
                   p
                   q
                   (- count 1)))))
