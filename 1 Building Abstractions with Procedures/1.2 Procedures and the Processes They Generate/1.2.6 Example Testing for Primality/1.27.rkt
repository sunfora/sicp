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
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

;test for Carmichael numbers
(define (successive-fermat-test? n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (iter i)
    (if (< i n)
      (and (try-it i)
           (iter (inc i)))
      #true))
  (iter 1))

(define (carmichael? n)
  (and (successive-fermat-test? n) 
       (not (prime? n))))