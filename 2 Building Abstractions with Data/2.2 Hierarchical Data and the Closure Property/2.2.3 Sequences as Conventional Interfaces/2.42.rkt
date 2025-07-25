#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (filter p seq)
  (accumulate 
    (lambda (x y)          
       (if (p x)             
         (cons x y)         
         y))                 
     nil                      
     seq))         

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (inc low) high))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (define (print-sizes)
    (display (map length (map queen-cols
              (enumerate-interval 1 board-size)))))
  (print-sizes)
  (queen-cols board-size))

(define empty-board nil)

(define (adjoin-position row col positions)
  (cons row positions))

(define (safe? k positions)
  (define (check-move step t positions)
    (or (null? positions)
      (let ((head (car positions))
            (tail (cdr positions))
            (next (+ t step)))
        (and (not (= next head))
             (check-move step next tail)))))
  (or (null? positions)
    (let ((head (car positions))
          (tail (cdr positions)))
      (and (check-move  1 head tail)
           (check-move  0 head tail)
           (check-move -1 head tail)))))

(define (display-row size)
  (lambda (i)
    (for-each 
      (lambda (j)
        (display (if (= i j) "Q" "#")))
      (enumerate-interval 1 size))
    (newline))) 

(define (display-queens positions)
  (for-each (display-row (length positions)) positions)
  (newline))
