#lang sicp

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) 
            (cdr items))))

(define (square-list items)
  (map square items))
