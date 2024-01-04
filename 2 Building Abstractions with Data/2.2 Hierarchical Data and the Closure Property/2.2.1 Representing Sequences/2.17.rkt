#lang sicp

(define (last-pair lst)
  (let ((tail (cdr lst)))
    (if (null? tail)
      lst
      (last-pair tail))))
