#lang sicp

(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display "guess: ") (display next) (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))


(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated fn t)
  (if (= 0 t)
    identity
    (compose fn (repeated fn (dec t)))))

(define (fixed-point-of-transform 
         g transform guess)
  (fixed-point (transform g) guess))


(define (expt x p)
  (if (= 0 p)
      1
      (* x (expt x (dec p)))))

(define (average-damped k p)
  (lambda (x)
    (fixed-point-of-transform
     (lambda (y) (/ x (expt y (dec p))))
     (repeated average-damp k)
     1.0)))

(define (nth-root x n)
  (let ((k (floor (log n 2))))
    ((average-damped k n) x)))
