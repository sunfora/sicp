#lang sicp

(define (repeated fn t)
  (if (= 0 t)
    identity
    (compose fn (repeated fn (dec t)))))


