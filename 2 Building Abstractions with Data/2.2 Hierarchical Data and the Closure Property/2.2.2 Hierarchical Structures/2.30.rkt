#lang sicp

(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) 
         (square tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))

(define (fork p f g)
  (lambda (x)
    (if (p x)
      (f x)
      (g x))))

(define (square-tmap tree)
  (map (fork pair? square-tmap square) tree))
