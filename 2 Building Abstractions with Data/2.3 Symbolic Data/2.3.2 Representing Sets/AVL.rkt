#lang sicp

;==== general functional things ====

(define (foldr op start seq)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
  (iter start seq))

(define (fork p? f g)
  (lambda (x)
    (if (p? x) 
      (f x) (g x))))

(define (-> . funcs)
  (foldr (lambda (r f)
           (lambda (x)
             (f (r x))))
         identity
         funcs))

;==== selectors & constructors ====
(define (make-tree entry left right)
  (list entry left right 
        (inc (max (height left)
                  (height right)))))

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

;==== various tree-operations shorthands ====

(define (on-left-branch f)
  (lambda (tree)
    (make-tree (entry tree)
               (f (left-branch tree))
               (right-branch tree))))

(define (on-right-branch f)
  (lambda (tree)
    (make-tree (entry tree)
               (left-branch tree)
               (f (right-branch tree)))))

(define (replace-right-branch tree branch)
  ((on-right-branch (lambda (right) branch)) tree))

(define (replace-left-branch tree branch)
  ((on-left-branch (lambda (left) branch)) tree))

;==== balancing ====

(define (rotate-left tree)
  (replace-left-branch 
    (right-branch tree)
    ((on-right-branch left-branch) tree)))

(define (rotate-right tree)
  (replace-right-branch 
    (left-branch tree)
    ((on-left-branch right-branch) tree)))

;; AVL-tree is considered balanced
;; if |(height right) - (height left)| < 1
(define (balanced? tree)
  (or (= -1 (balance-factor tree))
      (= 0 (balance-factor tree))
      (= 1 (balance-factor tree))))

;; apply left function if left is higher
;; or right function otherwise
(define (when-higher left-transform right-transform)
  (lambda (tree)
    ((if (>= (height (left-branch tree))
             (height (right-branch tree)))
       left-transform right-transform) tree)))


(define AVL-balance
  (fork balanced?
        identity
        (when-higher 
          (-> (on-left-branch 
                (when-higher 
                  identity rotate-left))
              rotate-right)
          (-> (on-right-branch 
                (when-higher 
                  rotate-right identity))
              rotate-left))))

;==== Set operations ====

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
  (define alter AVL-balance)
  (define (adjoin-x set) (adjoin-set x set))
  (define (default)
    (cond ((null? set) (make-tree x '() '()))
          ((= x (entry set)) set)
          ((< x (entry set))
           ((on-left-branch adjoin-x) set))
          ((> x (entry set))
           ((on-right-branch adjoin-x) set))))
  (alter (default)))

(define (leaf? set)
  (and (null? (left-branch set))
       (null? (right-branch set))))

(define (find-min set)
  (cond 
    ((null? set) '())
    ((null? (left-branch set))
     (entry set))
    (else (find-min (left-branch set)))))

(define (find-max set)
  (cond 
    ((null? set) '())
    ((null? (right-branch set))
     (entry set))
    (else (find-max (right-branch set)))))

(define (remove-head set)
  (cond ((leaf? set) '())
        ((null? (left-branch set)) 
         (right-branch set))
        ((null? (right-branch set))
         (left-branch set))
        (else
          (let ((m (find-min (right-branch set))))
            (make-tree m
                       (left-branch set)
                       (remove-set m (right-branch set)))))))

(define (remove-set x set)
  (define alter AVL-balance)
  (define (remove-x set)
    (remove-set x set))
  (define (default)
    (cond ((null? set) set)
          ((= x (entry set))
           (remove-head set))
          ((< x (entry set))
           ((on-left-branch remove-x) set))
          ((> x (entry set))
           ((on-right-branch remove-x) set))))
  (alter (default)))

;==== debuging ====

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

;==== examining height ====               
(#%require (only racket require
                        prefix-in
                        only-in))
(require (prefix-in rkt: 
                    (only-in racket
                             shuffle
                             in-inclusive-range
                             sequence->list)))
(require (prefix-in rkt: 
                    (only-in racket/mpair
                             mlist->list
                             list->mlist)))

(define (convert fn)
  (-> rkt:mlist->list
      fn
      rkt:list->mlist))

(define shuffle (convert rkt:shuffle)) 

(define (nums-seq n)
  (rkt:in-inclusive-range 1 n))

(define nums
  (-> nums-seq 
      rkt:sequence->list
      rkt:list->mlist))

(define (adjoin-values tree vals)
  (foldr (lambda (tree x)
           (adjoin-set x tree))
         tree
         vals))

(define (remove-values tree vals)
  (foldr (lambda (tree x)
           (remove-set x tree))
         tree
         vals))

(define phi (/ (inc (sqrt 5)) 2))

(define (max-height n)
  (- (inexact->exact 
       (round (log (* (+ 1 n) (sqrt 5)) phi))) 
     2))

(define (actual-height n)
  (height (adjoin-values '() (shuffle (nums n)))))

(define (min-height n)
  (inexact->exact (ceiling (log (inc n) 2))))

(define (test n)
  (string-append (number->string n) "\t"
                 (number->string (min-height n)) "\t"
                 (number->string (actual-height n)) "\t"
                 (number->string (max-height n)) "\n"))

(display (string-append "n\tmin\tactual\tmax\n"
                        (test 1)
                        (test 5)
                        (test 10)
                        (test 20)
                        (test 50)
                        (test 100)
                        (test 200)
                        (test 500)
                        (test 1000)
                        (test 2000)
                        (test 5000)
                        (test 10000)
                        (test 20000)
                        (test 50000)
                        (test 100000)
                        (test 200000)
                        (test 500000)
                        (test 1000000)))
