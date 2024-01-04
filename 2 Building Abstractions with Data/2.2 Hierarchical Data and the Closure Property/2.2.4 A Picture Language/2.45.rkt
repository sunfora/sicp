#lang sicp

(#%require sicp-pict)


(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter 
                                  (- n 1))))
        (below painter 
                (beside smaller smaller)))))

(define (split a b)
  (define (ab-split painter n)                 
    (if (= n 0)
        painter
        (let ((smaller (ab-split painter 
                                 (- n 1))))
          (a painter 
             (b smaller smaller)))))
  ab-split)
