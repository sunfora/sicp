#lang sicp

(define (deep-reverse lst)
  (define (push-all source dest)
    (if (null? source)
      dest
      (push-all (cdr source) (cons (deep-reverse (car source)) dest))))
  (if (list? lst)
      (push-all lst nil)
      lst))
