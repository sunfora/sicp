#lang sicp

; simple prime test 
(define (square x) (* x x))

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

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) 
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


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

(define (timed-fast-prime-test n)
  (newline)
  (display n)
  (start-fast-prime-test n (runtime)))

(define (start-fast-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) 
                       start-time))))

;; search-for-primes
(define (search-for-primes a b)
  (if (< a b)
    (begin 
      (if (not (even? a))
          (begin
            (timed-prime-test a)
            (timed-fast-prime-test a)))
      (search-for-primes (inc a) b))))