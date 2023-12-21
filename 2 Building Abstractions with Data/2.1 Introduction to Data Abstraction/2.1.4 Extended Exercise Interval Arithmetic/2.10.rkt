#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound i)
  (min (car i) (cdr i)))

(define (upper-bound i)
  (max (car i) (cdr i)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (interval->string x)
  (string-append "["
                 (number->string (lower-bound x))
                 ", "
                 (number->string (upper-bound x))
                 "]"))

(define (div-interval x y)
  (define (revert t)
    (if (not (= 0 t))
      (/ 1.0 t)
      (error (string-append "cannot divide by interval: " (interval->string y)))))
  (mul-interval x 
                (make-interval 
                 (revert (upper-bound y)) 
                 (revert (lower-bound y)))))

(define (sub-interval x y)
  (add-interval x
                (make-interval
                  (* -1 (upper-bound y))
                  (* -1 (lower-bound y)))))

(define (width x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

(define (print-interval x)
  (display (interval->string x))
  (newline))
