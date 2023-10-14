#lang sicp

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (* a b)
  (define (iter pa res i)
    (cond ((= 0 i)
           res)
          ((even? i)
           (iter (double pa) res (halve i)))
          (else (iter pa (+ pa res) (dec i)))))
  (iter a 0 b))
