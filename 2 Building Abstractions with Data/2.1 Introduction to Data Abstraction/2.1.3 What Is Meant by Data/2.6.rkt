#lang sicp

(define zero 
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two 
  (lambda (f) (lambda (x) (f (f x)))))

(define (church->number x)
  ((x inc) 0))

(define (sum a b)
  (lambda (f) (lambda (x)
    ((a f) ((b f) x)))))
