#lang sicp

(define (pascal-triangle row pos)
  (define (pt row pos) 
    (if (>= 1 row)
        (if (= 1 pos) 1 0)
        (+ (pt (dec row) pos) 
           (pt (dec row) (dec pos)))))
  (pt row pos))

(define (display-pascal k)
  (define (display-row k)
    (define (iter i)
      (if (<= i k)
          (begin 
            (display (pascal-triangle k i)) (display " ")
            (iter (inc i)))))
    (iter 1))
  (define (iter i)
    (if (<= i k)
        (begin
          (display-row i) (newline)
          (iter (inc i)))))
  (iter 1))

(display-pascal 6)