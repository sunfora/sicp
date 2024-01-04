#lang sicp

(define (fork p f g)
  (lambda (x) (if (p x) (f x) (g x))))

(define (tree-map f tree)
  (define (tree-map* tree)
    (tree-map f tree))
  (map (fork list? tree-map* f) tree))