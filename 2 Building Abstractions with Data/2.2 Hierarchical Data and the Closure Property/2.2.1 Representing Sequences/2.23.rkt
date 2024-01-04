#lang sicp

(define (for-each action sequence)
  (if (not (null? sequence))
    (begin 
      (action (car sequence))
      (for-each action (cdr sequence)))))
