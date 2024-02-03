#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (if (or (null? set1) 
          (null? set2))
    '()
    (let ((intersected (intersection-set 
                        (cdr set1)
                        set2))
          (elem (car set1)))
      (if (and (element-of-set? elem set2)
               (not (element-of-set? elem intersected)))
        (cons elem intersected)
        intersected))))

(define (drop-duplicates set)
  (intersection-set set set))

(define (union-set set1 set2)
  (append set1 set2))
