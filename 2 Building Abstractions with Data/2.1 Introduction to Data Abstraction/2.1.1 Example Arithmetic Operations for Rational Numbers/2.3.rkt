#lang sicp

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-rect cons)
(define low-corner car)
(define high-corner cdr)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
  (newline)


(define (rect-width rect)
  (- (x-point (high-corner rect)) 
     (x-point (low-corner rect))))

(define (rect-height rect)
  (- (y-point (high-corner rect)) 
     (y-point (low-corner rect))))

(define (perim rect)
  (let (a (rect-width rect))
       (b (rect-height rect))
       (* 2 (+ a b))))

(define (area rect)
  (let (a (rect-width rect))
       (b (rect-height rect))
       (* a b)))
