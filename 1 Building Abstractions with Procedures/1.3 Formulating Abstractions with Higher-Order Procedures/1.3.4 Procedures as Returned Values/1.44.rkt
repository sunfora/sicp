#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated fn t)
  (if (= 0 t)
    identity
    (compose fn (repeated fn (dec t)))))

(define (smooth fn dx)
  (lambda (x)
    (/ (+ (fn (- x dx)) (fn x) (fn (+ x dx)))
       3)))

(define (n-fold n fn dx)
  (define (smooth-dx fn)
    (smooth fn dx))
  ((repeated smooth-dx n) fn))

