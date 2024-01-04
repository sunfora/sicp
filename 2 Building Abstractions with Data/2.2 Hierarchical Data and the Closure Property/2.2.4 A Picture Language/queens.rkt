#lang sicp
(#%require sicp-pict)
(#%require racket/base)

(define black  (load-painter  "assets/cell_black.png"))
(define blackq (load-painter "assets/queen_black.png"))
(define white  (load-painter  "assets/cell_white.png"))
(define whiteq (load-painter "assets/queen_white.png"))

(define (beside-with-ratio s a b)
  (let ((right (transform-painter a(make-vect 0 0)
                                   (make-vect s 0)
                                   (make-vect 0 1)))
        (left (transform-painter b (make-vect s 0)
                                   (make-vect 1 0)
                                   (make-vect s 1))))
  (lambda (frame)
    (right frame)
    (left frame))))

(define (beside p . ps)
  (if (null? ps)
      p
      (let ((n (inc (length ps)))
          (rst (apply beside ps)))
      (beside-with-ratio (/ 1 n) p rst))))

(define (below p . ps)
  (rotate90 (apply beside (map rotate270 (cons p ps)))))

(define (enumerate-interval lower upper)
  (if (> lower upper)
      nil
      (cons lower (enumerate-interval (inc lower) upper))))

(define (grid coord->painter width height)
  (apply below
         (map (lambda (row)
                (apply beside
                       (map (lambda (col)
                              (coord->painter row col))
                            (enumerate-interval 1 width))))
              (enumerate-interval 1 height))))

(define (queens->painter board-size queens)
  (define (choose-cell color type)
    (let ((cell (+ type (* 2 color))))
      (list-ref (list white whiteq black blackq) cell)))
  (define (cell i j)
    (let ((p (cons i j)))
      (choose-cell (remainder (+ i j) 2)
                   (if (member p queens) 1 0))))
  (grid cell board-size board-size))

;; Queens

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

(define (queens->painter* solution)
  (define board-size (length solution))
  (define (rows->coords rows)
    (define n (length rows))
    (define (iter n from to)
      (if (null? from)
          to
          (iter (dec n) (cdr from)
                        (cons (cons (car from) n)
                              to))))
    (iter n rows nil))
  (queens->painter board-size
                   (rows->coords solution)))