#lang sicp

(define (entry tree) (cadr tree))
(define (left-branch tree) (car tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list left entry right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
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

(define trees-2.16
  (list '(((() 1 ()) 3 (() 5 ())) 7 (() 9 (() 11 ())))
        '((() 1 ()) 3 ((() 5 ()) 7 (() 9 (() 11 ()))))
        '(((() 1 ()) 3 ()) 5 ((() 7 ()) 9 (() 11 ())))))

(define (show fn)
  (display fn) 
  (newline)
  (for-each (lambda (lst)
              (display lst)
              (newline))
            (map fn trees-2.16)))

(show tree->list-1)
(show tree->list-2)

(define (loop n f)
  (if (zero? n)
    '()
    (begin (f)
           (loop (dec n) f))))

(define (timed fn . args)
  (define times 10.0)
  (let ((t (runtime)))
    (loop times (lambda () (apply fn args)))
    (/ (- (runtime) t)
       times)))

(define (left-bamboo n)
  (if (zero? n)
    '()
    (make-tree n
               (left-bamboo (dec n))
               '())))

(define (bamboo-test fn n)
  (timed fn (left-bamboo n)))

(define (test n)
  (let ((t-1 (bamboo-test tree->list-1 n))
        (t-2 (bamboo-test tree->list-2 n))) 
    (string-append
      (number->string n)   "\t"
      (number->string t-2) "\t"
      (number->string t-1) "\t"
      (number->string (round (/ t-1 t-2)))
      )))

(define (make-table a b step)
  (define (iter i)
    (if (< i b)
      (string-append (test i) "\n"
                     (iter (+ step i)))
      ""))
  (iter a))

(display 
  (string-append
    "size\tsecond\tfirst\tratio\n"
    (make-table 10   100   10 )     
    (make-table 100  1000  100)     
    (make-table 1000 10000 1000)))
