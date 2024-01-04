#lang sicp

(define (filter f lst)
  (if (null? lst)
    lst
    (let ((h (car lst))
          (t (cdr lst)))
      (if (f h) 
        (cons h (filter f t))
        (filter f t)))))

(define (same-parity a . b)
  (filter (lambda (x) (= (remainder x 2)
                         (remainder a 2)))
          (cons a b)))

