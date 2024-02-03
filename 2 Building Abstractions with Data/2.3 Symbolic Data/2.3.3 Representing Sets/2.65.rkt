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

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (intersection-set set1 set2)
  (list->tree (intersection-list (tree->list set1)
                                 (tree->list set2))))

(define (union-set set1 set2)
  (list->tree (union-list (tree->list set1)
                          (tree->list set2))))

(define (intersection-list lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (let ((x1 (car lst1)) (x2 (car lst2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-list 
                         (cdr lst1)
                         (cdr lst2))))
              ((< x1 x2) (intersection-list 
                          (cdr lst1) 
                          lst2))
              ((< x2 x1) (intersection-list 
                          lst1 
                          (cdr lst2)))))))

(define (union-list lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      (append lst1 lst2)
      (let ((x1 (car lst1)) (x2 (car lst2)))
        (cond ((= x1 x2)
               (cons x1 (union-list 
                         (cdr lst1)
                         (cdr lst2))))
              ((< x1 x2)
               (cons x1 (union-list 
                          (cdr lst1) 
                          lst2))) 
              ((> x1 x2)
               (cons x2 (union-list 
                          lst1 
                          (cdr lst2))))))))
