#lang sicp

(define (entry tree) (car tree))

(define (left-branch tree) 
  (if (null? tree) '() (cadr tree)))

(define (right-branch tree)
  (if (null? tree) '() (caddr tree)))

(define (height tree)
  (if (null? tree) 0 (cadddr tree)))

(define (balance-factor tree)
  (- (height (right-branch tree))
     (height (left-branch tree))))

(define (rotate-left tree)
  (make-tree (entry (right-branch tree))
             (make-tree (entry tree)
                        (left-branch tree)
                        (left-branch (right-branch tree)))
             (right-branch (right-branch tree))))
 
(define (rotate-right tree)
  (make-tree (entry (left-branch tree))
             (left-branch (left-branch tree))
             (make-tree (entry tree)
                        (right-branch (left-branch tree))
                        (right-branch tree))))

(define (rotate-right-left tree)
  (rotate-left 
    (make-tree (entry tree) 
               (left-branch tree)
               (rotate-right (right-branch tree)))))

(define (rotate-left-right tree)
  (rotate-right 
    (make-tree (entry tree) 
               (rotate-left (left-branch tree))
               (right-branch tree))))



(define test
  (make-tree 4 (make-tree 2 (make-tree 1 '() '()) 
                            (make-tree 3 '() '()))
               (make-tree 6 (make-tree 5 '() '())
                            (make-tree 7 '() '()))))

(define (display-tree tree)
  (define (pad n)
    (make-string n (string-ref " " 0)))
  (define (display-padded n str)
    (display (string-append (pad n)
                            str
                            "\n")))
  (define (display-tree-padded level tree)
    (if (null? tree)
      (display-padded level "x")
      (begin 
        (display-tree-padded 
          (+ level 4) 
          (left-branch tree))
        (display-padded 
          level 
          (number->string (entry tree)))
        (display-tree-padded 
          (+ level 4) 
          (right-branch tree)))))
  (display-tree-padded 0 tree))


(define (balanced? tree)
  (or (= -1 (balance-factor tree))
      (= 0 (balance-factor tree))
      (= 1 (balance-factor tree))))

(define (higher tree)
  (if (>= (height (left-branch tree))
          (height (right-branch tree)))
    'left
    'right))

(define (left-left? tree)
  (and (= -2 (balance-factor tree))
       (eq? 'left (higher (left-branch tree)))))

(define (left-right? tree)
  (and (= -2 (balance-factor tree))
       (eq? 'right (higher (left-branch tree)))))


(define (right-right? tree)
  (and (= 2 (balance-factor tree))
       (eq? 'right (higher (right-branch tree)))))

(define (right-left? tree)
  (and (= 2 (balance-factor tree))
       (eq? 'left (higher (right-branch tree)))))

(define (rotate tree)
  (cond ((balanced? tree) tree)
          ((left-left? tree) (rotate-right tree))
          ((left-right? tree) (rotate-left-right tree))
          ((right-left? tree) (rotate-right-left tree))
          ((right-right? tree) (rotate-left tree))))

(define (make-tree entry left right)
  (rotate (list entry left right 
                (inc (max (height left)
                          (height right))))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (foldr op start seq)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
  (iter start seq))

(display-tree (foldr (lambda (tree x) (adjoin-set x tree)) '() '(1 2 3 4 5 6 7 8
9 10 11 12 13 14 15)))
