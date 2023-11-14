## 1.9

первая программа будет исполняться примерно вот так:
```
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
```
а вторая 
```
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9
```

## 1.10
```
(A 1 10)
1024
```
```
(A 2 4)
65536
```

```
(A 3 3)
65536
```

(define (f n) (A 0 n))
ну тут всё понятно, оно всегда раскрывается в (* 2 n)
поэтому
f(n) = 2n

(define (g n) (A 1 n))
давайте один раз раскроем:
получим (A 1 n) -> (A 0 (A 1 (- n 1))) -> (* 2 (A 1 (- n 1)))
ну и понятно, что рекурсивно оно раскрывается в 2^n

g(n) = 2^n

(define (h n) (A 2 n))

(A 2 n) -> (A 1 (A 2 (- n 1))) = 2^(A 2 (- n 1))

ну то есть это башенка степеней 2^(2^(2^...^(2^0)...)
что в свою очередь записывается просто как тетрация 2↑↑(n - 1)

h(n) = 2↑↑(n - 1)

# 1.11

```racket
#lang sicp

; recursive
(define (f n) 
  (if (> n 3)
      (+ (* 1 (f (- n 1)))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))
      n))

; iterative
(define (g n)
  (define (calc z y x)
    (+ (* 1 x) (* 2 y) (* 3 z)))
  (define (g-iter f-3 f-2 f-1 i)
    (if (<= i n)
        (g-iter f-2 f-1 (calc f-3 f-2 f-1) (inc i))
	f-1))
  (if (> n 3)
      (g-iter 1 2 3 4)
      n))
```
# 1.12

```racket
#lang sicp

(define (pascal-triangle row pos)
  (define (pt row pos) 
    (if (>= 1 row)
        (if (= 1 pos) 1 0)
        (+ (pt (dec row) pos) 
           (pt (dec row) (dec pos)))))
  (pt row pos))

(define (display-pascal k)
  (define (display-row k)
    (define (iter i)
      (if (<= i k)
          (begin 
            (display (pascal-triangle k i)) (display " ")
            (iter (inc i)))))
    (iter 1))
  (define (iter i)
    (if (<= i k)
        (begin
          (display-row i) (newline)
          (iter (inc i)))))
  (iter 1))

(display-pascal 6)
```

## 1.13

ну давайте по индукции докажем 
fib n = (φ^n − ψ^n ) / sqrt(5)

база:

fib 0 = 0
fib 1 = (φ − ψ) / sqrt(5) = 1

пусть верно:
fib k = (φ^k − ψ^k ) / sqrt(5)
fib (k+1) = (φ^(k+1) − ψ^(k+1) ) / sqrt(5)

покажем, что fib k + fib (k + 1) = fib (k + 2)
в самом деле:

заметим, что 
φ^2 = 1 + φ
ψ^2 = 1 + ψ

но тогда:
fib k + fib (k + 1) = (φ^k(1 + φ) − ψ^k(1 + ψ)) / sqrt(5)
                    = (φ^(k + 2) − ψ^(k + 2)) / sqrt(5)
		    = fib (k + 2)

откуда и получаем, что fin (k + 2) = (φ^(k + 2) − ψ^(k + 2)) / sqrt(5)

что и требовалось доказать

осталось доказать, что fib n ближайшее число к φ^n / sqrt(5)
ну это достаточно тривиально делается:

заметим, что ψ = (1 - sqrt(5)) / 2 ≈ -0.618
но тогда |ψ| < 1, что в свою очередь уже даёт нам оценку

|ψ^n / sqrt(5)| < 1/2

но учитывая предыдущее доказательство, получим что

fib n + ψ^n / sqrt(5) = φ^n / sqrt(5)

однако мы уже знаем, что хвост в виде пси не больше 1/2
и поэтому округление фи всегда нам даст число фибоначи

такие дела

## 1.14

если честно, то мне было лень это самому делать :\
поэтому отсылаю вас сюда за картинкой:
https://sicp-solutions.net/post/sicp-solution-exercise-1-14/

## 1.15

ну первая часть задания делается тривиально
каждый раз у нас аргумент уменьшается в три раза
и когда он становится совсем маленьким, мы наконец заканчиваем

надо просто решить уравнение
x / 3^(k - 1) < 0.1

получаем, что это просто

10x < 3^(k - 1)

что в свою очередь даёт нам

1 + log3(10x) < k

ну и ответ можно получить через функцию:
(define (number-of-steps x)
	(ceiling (+ 1 (log (* 10 x) 3))))

вбив (number-of-steps 12.15) 
получим 6

вторая часть легко получается из первой, просто доказывая, что существенная часть во всём этом безобразии — логарифм
(ceiling (+ 1 (log (* 10 x) 3))) = O(log x)

если надо пояснить почему, я поясню, но в целом более менее и так понятно

## 1.16

```racket
#lang sicp
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-exp base p)
  (define (iter pb n result)
    (cond ((= 0 i) 
            result)
          ((even? n)
            (fast-exp (* pb pb) (/ n 2) result))
          (else
            (fast-exp pb (- n 1) (* pb result)))))
  (iter base p 1))

(fast-exp 2 4)
```

## 1.17

```racket
#lang sicp

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (* a b)
  (cond ((= 0 b)
         0)
        ((even? b)
         (double (* a (halve b))))
        (else (+ a (* a (- b 1))))))

```

## 1.18

```racket
#lang sicp

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (* a b)
  (define (iter pa res i)
    (cond ((= 0 i)
           res)
          ((even? i)
           (iter (double pa) res (halve i)))
          (else (iter pa (+ pa res) (dec i)))))
  (iter a 0 b))
```

## 1.19

ну короче матричное умножение по сути дела
только немного странно поданное

делаем T(p, q) * (T(p, q) * (1, 0))
и доказываем, что существует 

T(p', q') * (0, 1) = T(p, q) * (T(p, q) * (1, 0))

у меня получились следующие соотношения:

p' = (  p  )^2 + q^2
q' = (p + q)^2 - p^2

```racket
#lang sicp


(define (fib n)
  (fib-iter 1 0 0 1 n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; it can be shown that 
; p' = (  p  )^2 + q^2
; q' = (p + q)^2 - p^2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (recompute-q p q)
  (- (square (+ p q)) (square p)))

(define recompute-p sum-of-squares)

(define (fib-iter a b p q count)
  (cond ((= count 0) 
         b)
        ((even? count)
         (fib-iter a
                   b
                   (recompute-p p q)
                   (recompute-q p q)
                   (/ count 2)))
        (else 
         (fib-iter (+ (* b q) 
                      (* a q) 
                      (* a p))
                   (+ (* b p) 
                      (* a q))
                   p
                   q
                   (- count 1)))))
```

## 1.20

### applicative order
```
(gcd 206 40)
(gcd 40 (remainder 206 40)) *
(gcd 40 6)
(gcd 6 (remainder 40 6)) *
(gcd 6 4)
(gcd 4 (remainder 6 4)) *
(gcd 4 2)
(gcd 2 (remainder 4 2)) *
(gcd 2 0)
```
итого 4 раза вызвали

### normal order

вычисления при раскрытии у нас происходят только при if
поэтому количество вычислений remainder зависит от их количества в аргументе b
и в финальном аргументе a

```
(gcd 206 40)
...
0

(gcd 40 (remainder 206 40))
...
1

(gcd (remainder 206 40) 
     (remainder 40 (remainder 206 40)))
... 
3

(gcd (remainder 40 (remainder 206 40)) 
     (remainder (remainder 206 40) 
                (remainder 40 (remainder 206 40))))
...
7

(gcd (remainder (remainder 206 40) 
                (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40)) 
                (remainder (remainder 206 40) 
                           (remainder 40 (remainder 206 40)))))
...
14

(remainder (remainder 206 40) 
	   (remainder 40 (remainder 206 40)))
18
```

итого 18 вычислений

## 1.21
пропустим
просто делаем то, что нам говорят

## 1.22
просто делаем то, что нам говорят

```racket
#lang sicp

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

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

(define (search-for-primes a b)
  (if (< a b)
    (begin 
      (if (not (even? a)) (timed-prime-test a))
      (search-for-primes (inc a) b))))
```

## 1.23

```racket
#lang sicp

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

(define (search-for-primes a b)
  (if (< a b)
    (begin 
      (if (not (even? a)) (timed-prime-test a))
      (search-for-primes (inc a) b))))
```

код действительно быстрее работает, но конечно не в 2 раза, а примерно в 1.5
а всё потому, что мы тратим время на вызов функции и на branching, потому что заместо простенького (+ 1 x) у нас теперь достаточно сложное вычисление

## 1.24
тут в принципе всё хорошо, как и в 1.22
действительно работает примерно за логарифм

```racket 
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

```

## 1.25
о неет, большие числа имеют большой весьма оверхед
потому что, чтобы их эмулировать нам надо а) их где-то хранить б) уметь с ними работать

есть достаточно крутые алгоритмы для перемножения больших чисел, но конечно ни один из них по скорости не будет дотягивать до той скорости, которую мы достигаем, просто оставаясь в пределах модульной арифметики

можно позапускать и посмотреть насколько дольше требуется времени и памяти
но я этим не занимался

## 1.26
Ну, чтобы вычислить (при подставленных остальных параметрах) expmod(exp) нам требуется T(exp) времени

которое удовлетворяет уравнению

T(exp) >= 2 * T(exp / 2)

давайте предположим, что до некоторого числа T(exp) > Cexp (константу можно подобрать)

но тогда получим страшную вещь
для любого exp
T(exp) >= 2 * T(exp / 2) > 2 * С exp/2 > C exp

итого T(exp) = Ω(exp) 

то есть это получился как минимум линейный алгоритм
(ну можно доказать, что и как максимум)

## 1.27

```racket
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

```

## 1.28

```racket
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

```
