#lang sicp

(define (entry tree) (cadr tree))
(define (left-branch tree) (car tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list left entry right))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size      (quotient (- n 1) 2))
             (right-size     (- n (+ left-size 1)))
                             
             (left-result    (partial-tree elts left-size))
             (non-left-elts  (cdr left-result))
             (right-elts     (cdr non-left-elts))
             (right-result   (partial-tree right-elts right-size))

             (this-entry     (car non-left-elts))
             (left-tree      (car left-result))
             (right-tree     (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry 
                         left-tree 
                         right-tree)
              remaining-elts))))
