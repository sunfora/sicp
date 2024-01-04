#lang sicp

(define (reverse lst)
  (define (push-all source dest)
    (if (null? source)
      dest
      (push-all (cdr source) (cons (car source) dest))))
  (push-all lst nil))
