#lang sicp

(define (entry tree) (cadr tree))
(define (left-branch tree) (car tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list left entry right))

(define (make-entry key value)
  (list key value))

(define key car)
(define value cadr)

(define (lookup given-key set)
  (cond ((null? set) false)
        ((= given-key (key (entry set)))
         (entry set))
        ((< given-key (key (entry set)))
         (lookup given-key (left-branch set)))
        (else
         (lookup given-key (right-branch set)))))
