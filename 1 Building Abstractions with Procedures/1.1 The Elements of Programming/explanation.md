## 1.1 
no comments

## 1.2
no comments

## 1.3
очень простенькая программа
рассматриваем два случая получаем третий

```racket
#lang sicp

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f x y z)
  (cond ((and (<= x y) (<= x z))
          (sum-of-squares y z))
        ((and (<= y x) (<= y z))
          (sum-of-squares x z))
        (else
          (sum-of-squares x y))))
```

## 1.4

