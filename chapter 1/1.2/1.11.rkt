#lang sicp

; recursive
(define (f n) 
  (if (> n 3)
      (+ (* 1 (f (- n 1)))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))
      n))

; iterative
(define (g n)
  (define (calc z y x)
    (+ (* 1 x) (* 2 y) (* 3 z)))

  (define (g-iter f-3 f-2 f-1 i)
    (if (<= i n)
        (g-iter f-2 f-3 (calc f-3 f-2 f-1) (inc i))
	f-1))

  (if (> n 3)
      (g-iter 1 2 3 4)
      n))

