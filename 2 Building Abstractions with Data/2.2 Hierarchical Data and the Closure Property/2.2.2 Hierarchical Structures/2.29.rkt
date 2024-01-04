#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)
(define right-branch cadr)

(define branch-length car)
(define branch-structure cadr)

(define (branch-weight branch)
  (define mobile? pair?)
  (let ((structure (branch-structure branch)))
    (if (mobile? structure)
      (total-weight structure)
      structure)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) 
     (branch-weight (right-branch mobile))))

(define (balanced? mobile)
  (define (on-branch branch)
    (with-weight (branch-structure branch)))
  (define (with-weight mobile)
    (define (combine-results left right)
      (let ((l-balanced (car left))
            (l-weight (cdr left))
            (l-length (branch-length (left-branch mobile)))
            (r-balanced (car right))
            (r-weight (cdr right))
            (r-length (branch-length (right-branch mobile))))
        (cons (and l-balanced r-balanced
                 (= (* l-weight l-length)
                    (* r-weight r-length)))
              (+ l-weight r-weight))))
    (if (not (pair? mobile))
      (cons true mobile)
      (combine-results (on-branch (left-branch mobile))
                       (on-branch (right-branch mobile)))))
  (car (with-weight mobile)))
