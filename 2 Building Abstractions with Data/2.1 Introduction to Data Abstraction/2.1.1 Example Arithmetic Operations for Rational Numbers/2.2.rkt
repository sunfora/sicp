#lang sicp

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
  (newline)

(define (mid-point seg)
  (define (average x y)
    (/ (+ x y) 2))
  (let ((a (start-segment seg))
        (b (end-segment seg)))
    (make-point (average (x-point a) (x-point b))
                (average (y-point a) (y-point b)))))
