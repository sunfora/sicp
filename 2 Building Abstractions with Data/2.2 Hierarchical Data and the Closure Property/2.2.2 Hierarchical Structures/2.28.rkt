#lang sicp

(define (fringe tree)
  (define (leaf? x)
    (not (list? x)))
  (define (merge tree lst)
    (cond ((null? tree) lst)
          ((leaf? tree) (cons tree lst))
          (else (merge (car tree)
                       (merge (cdr tree) lst)))))
  (merge tree nil))

