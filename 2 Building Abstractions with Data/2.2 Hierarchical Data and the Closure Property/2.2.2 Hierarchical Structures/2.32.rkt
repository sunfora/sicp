#lang sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s)))
            (cons-first (lambda (lst) 
                          (cons (car s) lst))))
        (append rest (map cons-first rest)))))
