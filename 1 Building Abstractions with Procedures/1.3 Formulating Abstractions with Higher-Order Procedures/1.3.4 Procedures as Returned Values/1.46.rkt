#lang sicp

(define (iterative-improve good-enough? improve) 
  (define (iter guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
        guess
        (iter next))))
  iter)

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.0001) 

(define (tollerance-eq? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (average-damp fn)
  (lambda (x) (average x (fn x))))

(define (sqrt x)
  ((iterative-improve 
    tollerance-eq?
    (average-damp (lambda (y) (/ x y)))) 1.0))

(define (fixed-point f guess)
  ((iterative-improve
    tollerance-eq?
    (lambda (guess) (f guess)))
   guess))

