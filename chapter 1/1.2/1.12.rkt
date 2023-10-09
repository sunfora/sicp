#lang sicp

(define (pascal-triangle row pos)
  (define (pt row pos) 
    (if (>= 1 row)
        (if (= 1 pos) 1 0)
        (+ (pt (dec row) pos) 
           (pt (dec row) (dec pos)))))
  (pt row pos))

(display (pascal-triangle 1 1))  (newline) 
(display (pascal-triangle 2 1))  (display (pascal-triangle 2 2)) (newline)
(display (pascal-triangle 3 1))  (display (pascal-triangle 3 2)) (display (pascal-triangle 3 3)) (newline) 
(display (pascal-triangle 4 1))  (display (pascal-triangle 4 2)) (display (pascal-triangle 4 3)) (display (pascal-triangle 4 4)) (newline)