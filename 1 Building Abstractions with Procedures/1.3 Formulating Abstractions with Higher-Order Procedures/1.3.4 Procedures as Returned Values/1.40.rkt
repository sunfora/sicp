#lang sicp

(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display "guess: ") (display next) (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) 
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) 
               guess))

(define (cubic a b c)
  (define (t x k)
    (if (= k 0) 
      1
      (* x (t x (- k 1)))))
  (lambda (x) (+ (* 1 (t x 3))
                 (* a (t x 2))
                 (* b (t x 1))
                 (* c (t x 0)))))

