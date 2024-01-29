#lang sicp

(define (atomic? x)
  (not (pair? x)))

(define (equal? x y)
  (or
    (and (atomic? x) (atomic? y) 
         (eq? x y))
    (and (pair? x) (pair? y)
         (equal? (car x) (car y))
         (equal? (cdr x) (cdr y)))))

