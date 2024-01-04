#lang sicp
(#%require sicp-pict)
(#%require racket/base)

(define outline
  (segments->painter
   (vects->segments
    (list (make-vect 0 0)
          (make-vect 1 0)
          (make-vect 1 1)
          (make-vect 0 1)
          (make-vect 0 0)))))

(define cross
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1 1))
    (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define diamond
  (segments->painter
   (vects->segments
    (list (make-vect 0.5 0.0)
          (make-vect 1.0 0.5)
          (make-vect 0.5 1.0)
          (make-vect 0.0 0.5)
          (make-vect 0.5 0.0)))))

(define (accumulate op init values)
  (if (null? values)
      init
      (op (car values)
          (accumulate op init (cdr values)))))

(define (flatmap f lst)
  (accumulate append nil (map f lst)))

(define (dedup vs)
    (define (add h vs)
      (if (null? vs)
          (cons h nil)
          (if (equal? h (car vs))
              vs
              (cons h vs))))
    (if (null? vs)
        nil
        (add (car vs)
             (dedup (cdr vs)))))

(define wave-coords 
  '[({25.00 99.75} {28.50 89.12} 
     {33.00 76.12} {35.50 63.88}
     {36.12 53.38} {36.50 47.00}
     {34.62 45.00} {31.50 43.88}
     {27.38 46.25} {21.88 49.88}
     {17.50 52.38} {14.12 51.88}
     {07.75 47.88} {02.75 40.38}
     {00.00 36.00}              )
    ({64.75 99.88} {63.88 94.12} 
     {60.50 82.75} {57.25 75.38}
     {54.00 71.12} {51.50 69.50}
     {48.62 69.38} {46.00 72.62}
     {43.50 77.88} {40.62 84.88}
     {37.75 99.99}              )
    ({99.62 72.25} {95.38 67.00}
     {90.62 61.00} {81.88 53.12}
     {72.12 45.88} {65.62 44.75}
     {62.62 45.88} {62.38 51.00}
     {63.38 61.25} {66.75 73.38}
     {71.25 84.88} {77.75 99.75})
    ({55.00 00.12} {57.62 03.62}
     {60.00 07.50} {62.12 12.25}
     {63.38 15.88} {62.75 19.50}
     {59.88 22.38} {57.62 25.88}
     {56.50 29.25} {56.50 32.38}
     {59.50 33.38} {64.25 32.25}
     {68.50 32.12} {72.38 33.12}
     {77.00 35.88} {81.88 40.00}
     {88.62 47.62} {95.00 55.00}
     {99.75 62.38}              )
    ({00.50 25.62} {05.25 32.88}
     {06.62 34.12} {13.00 40.25}
     {18.62 41.25} {24.25 38.38}
     {30.25 32.62} {37.50 32.88}
     {41.00 33.62} {43.12 32.00}
     {41.50 26.50} {36.12 21.62}
     {34.38 16.62} {35.75 10.00}
     {41.88 00.38}              )])

(define (wave-coord->vector coord)
  (let ((v (apply make-vect coord)))
    (vector-sub (make-vect 1 1)
                (vector-scale 0.01 v))))

(define wave
  (segments->painter
      (flatmap vects->segments
        (map (lambda (coords) 
               (map wave-coord->vector coords))
             wave-coords))))
