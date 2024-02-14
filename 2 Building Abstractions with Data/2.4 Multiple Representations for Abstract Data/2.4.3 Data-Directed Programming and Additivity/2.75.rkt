#lang sicp

(#%require (only racket 
                 pi))
(define (square x) (* x x))

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imag-part) (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else
           (error 'make-from-mag-ang 
                  "unknown op ~a" op))))
  dispatch)

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error 'make-from-real-imag 
                  "unknown op ~a" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))

(define (add-complex z1 z2)
  (make-from-real-imag 
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag 
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang 
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang 
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

(define (display-complex z)
  (define (round-10 x)
    (/ (round (* (expt 10 10) x))
       (expt 10 10)))
  (for-each (lambda (x) 
              (display x) 
              (newline))
            (list (list 'magnitude (round-10 (magnitude z)))
                  (list 'angle (round-10 (angle z)))
                  (list 'real (round-10 (real-part z)))
                  (list 'imag (round-10 (imag-part z))))))

