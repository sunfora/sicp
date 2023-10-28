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
если b положительное число, то просто получим + в качестве результата if
иначе получим -, иными словами мы либо вычисляем a - b, если b отрицательное, и a + b иначе
что в свою очередь просто навсего a + |b|

## 1.5

### applicative order:
что будет происходить? сначала мы вычисляем результаты аргументов и только после вычисляем функцию
очевидно, что программа зависнет, потому что (test 0 (p)) будет вычислять (p) [бесконечный цикл]
прежде чем перейдёт к вычислению test

### normal order:
программа выдаст 0, потому что сначала мы всё раскрываем в качестве текстовых постановок до примитивных операций и только потом что-то вычисляем

результатом раскрытия будет

```racket
#lang sicp

(if (= 0 0)
    0
    (p))
```

что в свою очередь вычислится в 0, потому что второй аргумент if-а не вычисляется

## 1.6
ничего хорошего, ожидаемо, не произойдёт

почему? 
ну потому что вспоминаем, что модель по которой функционирует наш интерпретатор — applicative order

поэтому мы всегда будем вычислять сначала все аргументы, а потом функцию
из-за чего у нас возникнут очевидные проблемы в виде бесконечной рекурсии из-за третьего аргумента

## 1.7

напомню, что код который мы должны поправить таков:

```racket 
#lang sicp

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) 
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))
```

### маленькие числа:
что же с ними не так?

чисто экспериментально можно увидеть, что вычисление маленьких чисел
приводит нас примерно к одной и той же константе плюс минус погрешность

ну например попытка вычислить (sqrt 0.000000000001)
даст нам 0.031250000010656254

аналогичный же ответ почти даст нам любое другое число такого порядка
ну например (sqrt 0.00000000009)
даст нам 0.031250000959062496

wtf?
когда мы вычисляем good-enough? то что происходит?
мы смотрим на |guess^2 - x| < const

но если x слишком мал, то мы и получим, что остановка произойдёт примерно в районе 
guess < sqrt(const ± x) ~ sqrt(const)

sqrt(0.001) это как раз и есть та самая 0.0316227766 
вокруг которой всё и происходит

### большие числа
ну аналогично пусть x guess очень большие числа

guess^2 = y = a * 2^k
x = b * 2^k

тогда из-за того, что a и b это величины ограниченной точности
(a - b) * 2^k при большом k будет либо равна нулю (если повезло), либо никогда не будет меньше нашей const

поэтому вычисление просто зависнет и никогда не завершится

### фикс

нам надо сделать то, что попросили, оценивать насколько отличается каждый следующий guess от предыдущего
и делать это с помощью относительной погрешности, чтобы избавиться от влияния экспоненты

```racket
#lang sicp

(define (square x) (* x x))

(define (relative-error x y)
  (/ (abs (- x y))
     y))

(define (good-enough? previous-guess guess)
  (< (relative-error previous-guess guess) 0.001))

(define (sqrt-iter previous-guess guess x)
  (if (good-enough? previous-guess guess)
      previous-guess
      (sqrt-iter guess (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 (improve 1.0 x) x))
```

## 1.8

берем код из предыдущего задания и просто меняем improve на то, что нас попросили

```racket
#lang sicp

(define (square x) (* x x))

(define (relative-error x y)
  (/ (abs (- x y))
     y))

(define (good-enough? previous-guess guess)
  (< (relative-error previous-guess guess) 0.001))

(define (cbrt-iter previous-guess guess x)
  (if (good-enough? previous-guess guess)
      previous-guess
      (cbrt-iter guess (improve guess x) x)))

(define (improve y x)
  (/ (+ (/ x (square y)) (* 2.0 y)) 3.0))

(define (average x y)
  (/ (+ x y) 2))

(define (cbrt x)
  (cbrt-iter 1.0 (improve 1.0 x) x))
```
