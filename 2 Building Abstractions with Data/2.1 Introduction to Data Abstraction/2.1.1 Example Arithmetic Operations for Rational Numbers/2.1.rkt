#lang sicp

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))


(define (make-rat n d)
  (define (sign x)
    (if (< x 0) -1 1))
  (define (from-unsigned sign n d)
    (let ((g (gcd n d)))
      (cons (* sign (/ n g))
                    (/ d g))))
   (let ((m (* (sign n)
               (sign d))))
     (from-unsigned m (abs n) (abs d))))

(define numer car)
(define denom cdr)
