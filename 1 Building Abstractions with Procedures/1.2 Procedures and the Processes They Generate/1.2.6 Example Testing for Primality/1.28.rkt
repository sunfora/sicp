#lang sicp

(define (square x) (* x x))

; simple prime test 
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next x)
    (if (= 2 x) 3 (+ 2 x)))

  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


; fast prime test
(define (expmod base exp m)
  (define (signal-nontrivial-sqrt x)
    (define (test sqrt sqr)
      (if (and (not (or  (= 1       sqrt)
                         (= (dec m) sqrt)))
               (=  1 sqr ))
          0 sqr))
    (test x (remainder (square x) m)))
  (cond ((= exp 0) 1)
        ((even? exp)
         (signal-nontrivial-sqrt (expmod base (/ exp 2) m)))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (miller-test n)
  (define (try-it a)
    (= (expmod a (dec n) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-test n) 
         (fast-prime? n (- times 1)))
        (else false)))