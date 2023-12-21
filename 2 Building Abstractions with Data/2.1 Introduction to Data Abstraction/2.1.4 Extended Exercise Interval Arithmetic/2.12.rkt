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
  (define (pos? x) (<= 0 x))
  
  (define x- (lower-bound x))
  (define x+ (upper-bound x))
  (define y- (lower-bound y))
  (define y+ (upper-bound y))
  
  (define (signs? a b c d)
    (and (eq? a (pos? x-))
         (eq? b (pos? x+))
         (eq? c (pos? y-))
         (eq? d (pos? y+))))
  
  (define (++++?) (signs? #t #t #t #t))
  (define (-+++?) (signs? #f #t #t #t))
  (define (--++?) (signs? #f #f #t #t))

  (define (++-+?) (signs? #t #t #f #t))
  (define (-+-+?) (signs? #f #t #f #t))
  (define (---+?) (signs? #f #f #f #t))

  (define (++--?) (signs? #t #t #f #f))
  (define (-+--?) (signs? #f #t #f #f))
  (define (----?) (signs? #f #f #f #f))

  (define (++++) 
    (make-interval (* x- y-)  
                   (* x+ y+)))
  (define (-+++) 
    (make-interval (* x- y+)
                   (* x+ y+)))
  (define (--++) 
    (make-interval (* x- y+) 
                   (* x+ y-)))

  (define (++-+) 
    (make-interval (* y- x+)
                   (* y+ x+)))
  (define (-+-+) 
    (make-interval (min (* x- y+) (* x+ y-)) 
                   (max (* x- y-) (* x+ y+))))
  (define (---+) 
    (make-interval (* x- y+) 
                   (* x- y-)))

  (define (++--) 
    (make-interval (* y- x+) 
                   (* y+ x-)))
  (define (-+--) 
    (make-interval (* y- x+) 
                   (* y- x-)))
  (define (----) 
    (make-interval (* x+ y+) 
                   (* x- y-)))

  (cond
    ((++++?) (++++))
    ((-+++?) (-+++))
    ((--++?) (--++))
                   
    ((++-+?) (++-+))
    ((-+-+?) (-+-+))
    ((---+?) (---+))
                   
    ((++--?) (++--))
    ((-+--?) (-+--))
    ((----?) (----))))

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

(define (eq-interval? a b)
  (and (= (lower-bound a) (lower-bound b))
       (= (upper-bound a) (upper-bound b))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (make-center-percent c p)
  (make-center-width c
                     (/ (* c p) 
                        100.0)))

(define (percent i)
  (* 100.0 (/ (width i)
              (center i))))
