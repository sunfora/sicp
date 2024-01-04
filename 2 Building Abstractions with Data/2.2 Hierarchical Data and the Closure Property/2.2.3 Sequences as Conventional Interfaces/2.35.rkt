#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (->1 x) 1)

(define (fork p f g)
  (lambda (x) (if (p x) (f x) (g x))))

(define (count-leaves t)
  (accumulate + 0 (map (fork list? count-leaves ->1) t)))

