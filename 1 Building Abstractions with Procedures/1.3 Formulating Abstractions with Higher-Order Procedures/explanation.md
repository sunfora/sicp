итак, в этот раз очень много маленьких домашек
которые особо комментировать нет смысла, кроме как предоставив код

и есть лишь парочка каких-то содержательных, которые собственно ниже разобраны

## 1.29
```racket
#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n))
           (y k))
          ((even? k)
           (* 2 (y k)))
          (else 
            (* 4 (y k)))))
  (* (/ h 3) (sum term 0 inc n)))
```

## 1.30
```racket
#lang sicp

(define (sum term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

```

## 1.31
```racket
#lang sicp

(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-product n)                               ; k = 1 2 3 4 5
  (define (even k) (* 2 k))                          ;     2 4 6 8 10
  (define (odd k) (dec (even k)))                    ;     1 3 5 7 9
  (define (term k) (/ (* (even (dec k)) (even k))    
                      (* (odd k) (odd k))))
  (* 4.0 (product term 2 inc (inc n))))
```

## 1.32
```racket
#lang sicp

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate-recursive combiner null-value term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

```
## 1.33
```racket
#lang sicp

(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (filtered-combiner a r)
    (if (predicate a) 
      (combiner (term a) r) 
      r))
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (filtered-combiner a result))))
  (iter a null-value))

(define (square x) (* x x))

(define (prime? n)
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

  (and (not (= 1 n))
       (= n (smallest-divisor n))))

(define (sum-of-squares-prime a b)
  (filtered-accumulate prime? + 0 square a inc b))

(define (product-gcd n)
  (define (coprime? x) 
    (= 1 (gcd x n)))
  (filtered-accumulate coprime? * 1 identity 1 inc n))
```

## 1.34
получим серию подстановок
```
(f f)
(f 2)
(2 2)
```

после чего получим ошибку, потому что 2 не процедура

## 1.35
```racket
#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define phi 
  (fixed-point (lambda (x) (+ 1.0 (/ 1 x))) 1))
```

## 1.36
```racket
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

(define (f x) (/ (log 1000) (log x)))

(fixed-point f 5)
(fixed-point (average-damp f) 5)

```

решение уравнения ≈ 4.5555
а количество итераций выглядит примерно вот так (как видно, с average damp всё гораздо быстрее сходится):

```
guess: 4.29202967422018
guess: 4.741863119908242
guess: 4.438204569837609
guess: 4.635299887107611
guess: 4.50397811613643
guess: 4.589989462723705
guess: 4.53301150767844
guess: 4.570475672855484
guess: 4.545720389670642
guess: 4.562024936588171
guess: 4.551263234080531
guess: 4.55835638768598
guess: 4.553676852183342
guess: 4.55676216434628
guess: 4.554727130670954
guess: 4.556069054770006
guess: 4.555184018843625
guess: 4.5557676565438205
guess: 4.555382746639082
guess: 4.55563658243586
guess: 4.555469180245326
guess: 4.555579577900997
guess: 4.5555067722873686
guess: 4.5555547860484085
guess: 4.555523121789556
guess: 4.555544003742869
guess: 4.555530232469306
guess: 4.555539314360711
guess: 4.555533325019961
guess: 4.555537274877186
guess: 4.555534670019515
guess: 4.555536387874308
guess: 4.5555352549810255
guess: 4.555536002102961
4.555536002102961
guess: 4.64601483711009
guess: 4.571611286076025
guess: 4.558294317536066
guess: 4.556006022881116
guess: 4.555615799731297
guess: 4.555549342575593
guess: 4.555538027101999
guess: 4.5555361005218895
guess: 4.555535772503211
4.555535772503211
```
## 1.37
```racket
#lang sicp

(define (cont-frac-recursive N D k)
  (define (recur i)
    (if (< i k)
        (/ (N i)
           (+ (D i) (recur (inc i))))
        (/ (N i)
           (D i))))
  (recur 1))

(define (cont-frac N D k)
  (define (iter result i)
    (if (< i k)
        (iter (/ (N (- k i))
                 (+ (D (- k i)) result))
              (inc i))
        result))
  (iter 0 0))

(define (phi k)
  (cont-frac
    (lambda (i) 1.0)
    (lambda (i) 1.0)
    k))
```
тоже ничего сложного по-сути
но можно немножко запутаться

## 1.38
```racket
#lang sicp

(define (cont-frac N D k)
  (define (iter result i)
    (if (< i k)
        (iter (/ (N (- k i))
                 (+ (D (- k i)) result))
              (inc i))
        result))
  (iter 0 0))

(define (e k) (+ 2.0 (cont-frac
                    (lambda (i) 1)
                    (lambda (i) 
                           (if (= 2 (remainder i 3))
                               (* 2 (/ (+ i 1) 3))
                               1))
                         k)))
```
## 1.39
```racket
#lang sicp

(define (cont-frac N D k)
  (define (iter result i)
    (if (< i k)
        (iter (/ (N (- k i))
                 (+ (D (- k i)) result))
              (inc i))
        result))
  (iter 0 0))

(define (tan-cf x k)
  (exact->inexact (cont-frac 
    (lambda (i) 
      (if (= 1 i)
        x
        (- (* x x))))
    (lambda (i) 
      (dec (* 2 i)))
    k)))
```
ну я тут дополнительно привожу ответ к float
но можно это исправить проще, просто заменой 2 → 2.0, 1 → 1.0

## 1.40
```racket
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
  (lambda (x) (+ (* 1 (expt x 3))
                 (* a (expt x 2))
                 (* b (expt x 1))
                 (* c (expt x 0)))))

```

ну можно проверить, что действительно
```
> (newtons-method (cubic 1 1 1) 1)
-0.9999999999997795
```

в то время как действительно корнем x^3 + x^2 + x + 1 = 0 является -1

## 1.41
```racket
#lang sicp

(define (double f)
  (lambda (x) (f (f x))))
```
причем: 
```
> (((double (double double)) inc) 5)
21
```

ну действительно, что происходит:
```
(((double (double double)) inc) 5)
```

имеет примерно следующее поведение
```
(double double) -> (lambda (f) (double (double f)))
                -> (lambda (f) (double (lambda (x) (f(f x)))))
		-> (lambda (f) (lambda (x) ((lambda (x) (f(f x))) ((lambda (x) (f(f x))) x))
		-> (lambda (f) (lambda (x) ((lambda (x) (f(f x))) (f(f x)))))
		-> (lambda (f) (lambda (x) (f(f(f(f x))))))
```

ну и получаем, что
```
(double (lambda (f) (lambda (x) (f(f(f(f x)))))))
```
это на самом деле 16-ая степень функции, потому что мы делаем double от четвертой степени
4 * 4 = 16
```
(lambda (f) (lambda (x) f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(x))))))))))))))))))
```

то есть 16 повторений функции inc
что эквивалентно ```(+ 16 x)```

## 1.42
```racket
#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))
```

## 1.43
```racket
#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated fn t)
  (if (= 0 t)
    identity
    (compose fn (repeated fn (dec t)))))
```

## 1.44
```
#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated fn t)
  (if (= 0 t)
    identity
    (compose fn (repeated fn (dec t)))))

(define (smooth fn dx)
  (lambda (x)
    (/ (+ (fn (- x dx)) (fn x) (fn (+ x dx)))
       3)))

(define (n-fold n fn dx)
  (define (smooth-dx fn)
    (smooth fn dx))
  ((repeated smooth-dx n) fn))
```

## 1.45
```racket
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


(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated fn t)
  (if (= 0 t)
    identity
    (compose fn (repeated fn (dec t)))))

(define (fixed-point-of-transform 
         g transform guess)
  (fixed-point (transform g) guess))


(define (expt x p)
  (if (= 0 p)
      1
      (* x (expt x (dec p)))))

(define (average-damped k p)
  (lambda (x)
    (fixed-point-of-transform
     (lambda (y) (/ x (expt y (dec p))))
     (repeated average-damp k)
     1.0)))

(define (nth-root x n)
  (let ((k (floor (log n 2))))
    ((average-damped k n) x)))
```

написать nth-root с разным уровнем вложенности average-damp не сложно
просто используем все маленькие примитивы построенные до этого момента

в моём случае это ```(average-damped k p)```, где k отвечает за степень average-damp, а p за число

и после этого здесь нам потребуется немного эксперимента

но в принципе быстро можно заметить, что после примерно каждой степени двойки n
со старым k, у нас всё зацикливается

поэтому мы ищем примерно какой-то двоичный логарифм от n
и на практике оно прекрасно работает

я правда не особо знаю почему
это какой-то numerical analysis, за который я не особо шарю

если у вас есть какие-то содержательные идеи на этот счёт
было бы интересно послушать, let me know, как говорится

## 1.46
```racket
#lang sicp

(define (iterative-improve good-enough? improve) 
  (define (iter guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
        guess
        (iter next))))
  iter)

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.0001) 

(define (tollerance-eq? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (average-damp fn)
  (lambda (x) (average x (fn x))))

(define (sqrt x)
  ((iterative-improve 
    tollerance-eq?
    (average-damp (lambda (y) (/ x y)))) 1.0))

(define (fixed-point f guess)
  ((iterative-improve
    tollerance-eq?
    (lambda (guess) (f guess)))
   guess))
```

достаточно душноватое задание
просто копипастим старые definition и абстрагируем их, как сказали

в принципе можно скипнуть
