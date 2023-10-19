#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated fn t)
  (if (= 0 t)
    identity
    (compose fn (repeated fn (dec t)))))


