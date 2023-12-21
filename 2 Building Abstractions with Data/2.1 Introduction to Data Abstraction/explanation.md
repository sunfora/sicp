## 2.1
```racket
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

```

## 2.2
```racket
#lang sicp

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
  (newline)

(define (mid-point seg)
  (define (average x y)
    (/ (+ x y) 2))
  (let ((a (start-segment seg))
        (b (end-segment seg)))
    (make-point (average (x-point a) (x-point b))
                (average (y-point a) (y-point b)))))

```

## 2.3
```racket 
#lang sicp

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-rect cons)
(define low-corner car)
(define high-corner cdr)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
  (newline)


(define (rect-width rect)
  (- (x-point (high-corner rect)) 
     (x-point (low-corner rect))))

(define (rect-height rect)
  (- (y-point (high-corner rect)) 
     (y-point (low-corner rect))))

(define (perim rect)
  (let (a (rect-width rect))
       (b (rect-height rect))
       (* 2 (+ a b))))

(define (area rect)
  (let (a (rect-width rect))
       (b (rect-height rect))
       (* a b)))

```

## 2.4
```racket 
#lang sicp

(define (cons x y) 
  (lambda (m) (m x y)))

(define (car z) 
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))
```

вообще идея пары представлять подобным образом восходит к [лямбда-исчислению](https://en.wikipedia.org/wiki/Lambda_calculus)
а в частности к [church-encoding](https://en.wikipedia.org/wiki/Church_encoding)

true  = λx.λy.x
false = λx.λy.y
pair  = λx.λy.λm xy
or    = λx.λy.xxy
and   = λx.λy.xyx

и т.д.

## 2.5
```racket
#lang sicp

(define (expt* x p)
  (if (= p 0)
      1
      (* x (expt* x (dec p)))))

(define (divides? x y)
  (= 0 (remainder x y)))

(define (div-times x y)
  (if (divides? x y)
    (inc (div-times (/ x y) y))
    0))

(define (cons x y)
  (* (expt* 2 x) (expt* 3 y)))

(define (car x)
  (div-times x 2))

(define (cdr x)
  (div-times x 3))
```

а это уже так называемая Гёделевская энумерация
не то, чтобы в ней есть что-то необычное

в принципе её достаточно легко придумать

но прикол в том, что создавалась (как и чарчевская) она в общем-то для совсем другой задачи
а конкретно для доказательств теорем Гёделя неполноты формальной арифметики

ведь действительно, если мы можем с помощью чисел хранить списки чисел
и доставать элементы данные с помощью арифметических функций

то мы можем закодировать произвольный текст
а в частности мы можем закодировать какое-нибудь утверждение формальной арифметики

и получается такая рефлексия, которая приводит к соотстветствующим проблемам
невыразимости и недоказуемости всякий вещей

## 2.6
```racket
#lang sicp

(define zero 
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two 
  (lambda (f) (lambda (x) (f (f x)))))

(define (church->number x)
  ((x inc) 0))

(define (sum a b)
  (lambda (f) (lambda (x)
    ((a f) ((b f) x)))))

```

для простоты я перейду к лямбда исчислению
потому что просто меньше писать

но по сути это та же самая subsitution model
```
zero = λf.λx.x
add-1 = λn.λf.λx.f(n f)x

add-1 zero = (λn.λf.λx.f(n f)x) (λf.λx.x)
           =  λf.λx.f((λf.λx.x) f)x
           =  λf.λx.f(λx.x)x
           =  λf.λx.fx

one = λf.λx.fx
```

ну или на языке lisp
```
(define one
  (lambda (f) (lambda (x) (f x))))
```

аналогично можно заметить, что 
add-1 one даст нам λf.λx.f(fx)

можно по логике добавить простенький конвертор
из нумералов, в нормальные числа
```
(define (church->number x)
  ((x inc) 0))
```

в частности получаем весьма ожидаемое поведение
```
(church->number two)
> 2
(church->number (add-1 two))
> 3
(church->number (add-1 (add-1 two)))
> 4
```

с суммой можно заметить следующее
раз числа это функции формата 

4 = λf.λx.(f(f(f(fx))))
2 = λf.λx.f(fx)

то достаточно сделать функцию
которая подсовывает одному числу аргумент второго

```
sum = λa.λb.
        λf.λx.(((a f)((b f) x)) x)
```

что тут происходит?
мы получаем 2 числа a, b

а далее создаём новое число
для этого запрашиваем еще 2 переменных f x

в этом конексте делаем подстановку f, x в число b ((b f) x)
а затем после подстановки f в а, заместо x подставляем b с заменой f x

а это буквально то, что мы хотели

проверить магию можно просто вызвав 2 + 2, получив 4
```
(church->number (sum two two))
> 4
```

## 2.7
```racket
#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound i)
  (min (car i) (cdr i)))

(define (upper-bound i)
  (max (car i) (cdr i)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define (print-interval x)
  (display "[") 
      (display (lower-bound x))
      (display ", ")
      (display (upper-bound x))
  (display "]"))
```

немножечко неэффективно
всё же позиционировать правильно (lower, upper) надо в конструкторе

и тогда селекторы будут просто аллиасами car, cdr

но так как нам конструктор изначально дали
то мы это делаем в селекторах

## 2.8
```racket
#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound i)
  (min (car i) (cdr i)))

(define (upper-bound i)
  (max (car i) (cdr i)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (add-interval x
                (make-interval
                  (* -1 (upper-bound y))
                  (* -1 (lower-bound y)))))

(define (print-interval x)
  (display "[") 
      (display (lower-bound x))
      (display ", ")
      (display (upper-bound x))
  (display "]"))
```

## 2.9
```racket
#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound i)
  (min (car i) (cdr i)))

(define (upper-bound i)
  (max (car i) (cdr i)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (add-interval x
                (make-interval
                  (* -1 (upper-bound y))
                  (* -1 (lower-bound y)))))

(define (width x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

(define (print-interval x)
  (display "[") 
      (display (lower-bound x))
      (display ", ")
      (display (upper-bound x))
  (display "]"))

```

ну первый факт доказывается на самом деле тривиально
по определению получаем
```[a, b] + [c, d] = [a + c, b + d]```

но в таком случае
```
width [a, b] + [c, d] = width [a + c, b + d] 
                      = (b + d - a - c) / 2 
                      = (b - a) / 2 + (d - c) / 2
                      = width [a, b] + width [c, d]
```

для второго же нам потребуется изобрести 2 разных кейса
при которых width x = width x', width y = width y',
но неверно что width xy = width x'y'

```
> (define x (make-interval 0.5 1))
> (define y (make-interval 0.3 1))
> (define x^ (make-interval 1.5 2))
> (define y^ (make-interval 1.3 2))
> (width x)
0.25
> (width y)
0.35
> (width x^)
0.25
> (width y^)
0.35
> (width (mul-interval x y))
0.425
> (width (mul-interval x^ y^))
1.025
> (width (div-interval x y))
1.4166666666666667
> (width (div-interval x^ y^))
0.39423076923076916
```

ну как можно заметить несмотря на то
что (width x, width y) = (width x', width y')
width x * y != width x' * y'

а значит width x * y не выразим в терминах некоторой функции f(width x, width y)
потому что иначе 
```
width x * y = f(width x, width y) 
            = f(width x', width y') 
            = width x' * y'
```
противоречие.

## 2.10
```racket
#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound i)
  (min (car i) (cdr i)))

(define (upper-bound i)
  (max (car i) (cdr i)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (interval->string x)
  (string-append "["
                 (number->string (lower-bound x))
                 ", "
                 (number->string (upper-bound x))
                 "]"))

(define (div-interval x y)
  (define (revert t)
    (if (not (= 0 t))
      (/ 1.0 t)
      (error (string-append "cannot divide by interval: " (interval->string y)))))
  (mul-interval x 
                (make-interval 
                 (revert (upper-bound y)) 
                 (revert (lower-bound y)))))

(define (sub-interval x y)
  (add-interval x
                (make-interval
                  (* -1 (upper-bound y))
                  (* -1 (lower-bound y)))))

(define (width x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

(define (print-interval x)
  (display (interval->string x))
  (newline))
```

## 2.11
```racket
#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound i)
  (min (car i) (cdr i)))

(define (upper-bound i)
  (max (car i) (cdr i)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (define (pos? x) (<= 0 x))
  
  (define x- (lower-bound x))
  (define x+ (upper-bound x))
  (define y- (lower-bound y))
  (define y+ (upper-bound y))
  
  (define (signs? a b c d)
    (and (eq? a (pos? x-))
         (eq? b (pos? x+))
         (eq? c (pos? y-))
         (eq? d (pos? y+))))
  
  (define (++++?) (signs? #t #t #t #t))
  (define (-+++?) (signs? #f #t #t #t))
  (define (--++?) (signs? #f #f #t #t))

  (define (++-+?) (signs? #t #t #f #t))
  (define (-+-+?) (signs? #f #t #f #t))
  (define (---+?) (signs? #f #f #f #t))

  (define (++--?) (signs? #t #t #f #f))
  (define (-+--?) (signs? #f #t #f #f))
  (define (----?) (signs? #f #f #f #f))

  (define (++++) 
    (make-interval (* x- y-)  
                   (* x+ y+)))
  (define (-+++) 
    (make-interval (* x- y+)
                   (* x+ y+)))
  (define (--++) 
    (make-interval (* x- y+) 
                   (* x+ y-)))

  (define (++-+) 
    (make-interval (* y- x+)
                   (* y+ x+)))
  (define (-+-+) 
    (make-interval (min (* x- y+) (* x+ y-)) 
                   (max (* x- y-) (* x+ y+))))
  (define (---+) 
    (make-interval (* x- y+) 
                   (* x- y-)))

  (define (++--) 
    (make-interval (* y- x+) 
                   (* y+ x-)))
  (define (-+--) 
    (make-interval (* y- x+) 
                   (* y- x-)))
  (define (----) 
    (make-interval (* x+ y+) 
                   (* x- y-)))

  (cond
    ((++++?) (++++))
    ((-+++?) (-+++))
    ((--++?) (--++))
                   
    ((++-+?) (++-+))
    ((-+-+?) (-+-+))
    ((---+?) (---+))
                   
    ((++--?) (++--))
    ((-+--?) (-+--))
    ((----?) (----))))

(define (interval->string x)
  (string-append "["
                 (number->string (lower-bound x))
                 ", "
                 (number->string (upper-bound x))
                 "]"))

(define (div-interval x y)
  (define (revert t)
    (if (not (= 0 t))
      (/ 1.0 t)
      (error (string-append "cannot divide by interval: " (interval->string y)))))
  (mul-interval x 
                (make-interval 
                 (revert (upper-bound y)) 
                 (revert (lower-bound y)))))

(define (sub-interval x y)
  (add-interval x
                (make-interval
                  (* -1 (upper-bound y))
                  (* -1 (lower-bound y)))))

(define (width x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

(define (print-interval x)
  (display (interval->string x))
  (newline))

(define (eq-interval? a b)
  (and (= (lower-bound a) (lower-bound b))
       (= (upper-bound a) (upper-bound b))))

(define (test)
  (define (test n x y z)
    (let ((x^ (apply make-interval x))
          (y^ (apply make-interval y))
          (z^ (apply make-interval z)))
      (let ((r^ (mul-interval x^ y^)))
        (if (eq-interval? r^ z^)
          (display (string-append "[Test " (number->string n) "]: OK \n"))
          (error   (string-append "[Test " (number->string n) "]: Failed \n"
                                  "Expected: \n" 
                                      (interval->string x^) " * " (interval->string y^) 
                                      " = " (interval->string z^) "\n"
                                  "But got: " (interval->string r^)))))))
    (test 1 '( 1  2) '( 3  4) '( 3  8))
    (test 2 '(-1  2) '( 3  4) '(-4  8))
    (test 3 '(-2 -1) '( 3  4) '(-8 -3))
    (test 4 '( 1  2) '(-3  4) '(-6  8))
    (test 5 '(-1  2) '(-3  4) '(-6  8))
    (test 6 '(-2 -1) '(-3  4) '(-8  6))
    (test 7 '( 1  2) '(-4 -3) '(-8 -3))
    (test 8 '(-1  2) '(-4 -3) '(-8  4))
    (test 9 '(-2 -1) '(-4 -3) '( 3  8))
    (display "Tests passed. \n"))

(test)

(define (gen-test n a b c d)
  (define (interval x)
    (display "'(")
    (display (lower-bound x))
    (display " ")
    (display (upper-bound x))
    (display ")"))
  (let ((x (make-interval a b))
        (y (make-interval c d)))
    (display "(test ")
    (display n) (display " ")
    (interval x) (display " ")
    (interval y) (display " ")
    (interval (mul-interval x y)) (display ")\n")))

(define (gen-tests)
  (gen-test 1 +1 +2 +3 +4) ;++ ++
  (gen-test 2 -1 +2 +3 +4) ;-+ ++
  (gen-test 3 -1 -2 +3 +4) ;-- ++
  (gen-test 4 +1 +2 -3 +4) ;++ -+
  (gen-test 5 -1 +2 -3 +4) ;-+ -+
  (gen-test 6 -1 -2 -3 +4) ;-- -+
  (gen-test 7 +1 +2 -3 -4) ;++ --
  (gen-test 8 -1 +2 -3 -4) ;-+ --
  (gen-test 9 -1 -2 -3 -4) ;-- --
  )
```

безумное задание, я советую вооружиться каким-нибудь редактором типа vim
потому что тут много копипасты и по-сути своей таблиц

итак в чём соль?
у нас есть 9 случаев в зависимости от знака
```
++ ++
-+ ++
-- ++
++ -+
-+ -+
-- -+
++ --
-+ --
-- --
```

каждый из них надо в конечном счёте разобрать
и предоставить максимально эффективную реализацию

для того, чтобы это было удобнее делать 
я предлагаю сначала запилить тесты

```racket
#lang sicp

(define (test)
  (define (test n x y z)
    (let ((x^ (apply make-interval x))
          (y^ (apply make-interval y))
          (z^ (apply make-interval z)))
      (let ((r^ (mul-interval x^ y^)))
        (if (eq-interval? r^ z^)
          (display (string-append "[Test " (number->string n) "]: OK \n"))
          (error   (string-append "[Test " (number->string n) "]: Failed \n"
                                  "Expected: \n" 
                                      (interval->string x^) " * " (interval->string y^) 
                                      " = " (interval->string z^) "\n"
                                  "But got: " (interval->string r^)))))))
    (test 1 '( 1  2) '( 3  4) '( 3  8))
    (test 2 '(-1  2) '( 3  4) '(-4  8))
    (test 3 '(-2 -1) '( 3  4) '(-8 -3))
    (test 4 '( 1  2) '(-3  4) '(-6  8))
    (test 5 '(-1  2) '(-3  4) '(-6  8))
    (test 6 '(-2 -1) '(-3  4) '(-8  6))
    (test 7 '( 1  2) '(-4 -3) '(-8 -3))
    (test 8 '(-1  2) '(-4 -3) '(-8  4))
    (test 9 '(-2 -1) '(-4 -3) '( 3  8))
    (display "Tests passed. \n"))
```

как видно тут тоже своеобразная таблица входов-выходов
поэтому я с помощью старой версии mul-interval сгенерировал таблицу тестов

```racket
(define (gen-test n a b c d)
  (define (interval x)
    (display "'(")
    (display (lower-bound x))
    (display " ")
    (display (upper-bound x))
    (display ")"))
  (let ((x (make-interval a b))
        (y (make-interval c d)))
    (display "(test ")
    (display n) (display " ")
    (interval x) (display " ")
    (interval y) (display " ")
    (interval (mul-interval x y)) (display ")\n")))

(define (gen-tests)
  (gen-test 1 +1 +2 +3 +4) ;++ ++
  (gen-test 2 -1 +2 +3 +4) ;-+ ++
  (gen-test 3 -1 -2 +3 +4) ;-- ++
  (gen-test 4 +1 +2 -3 +4) ;++ -+
  (gen-test 5 -1 +2 -3 +4) ;-+ -+
  (gen-test 6 -1 -2 -3 +4) ;-- -+
  (gen-test 7 +1 +2 -3 -4) ;++ --
  (gen-test 8 -1 +2 -3 -4) ;-+ --
  (gen-test 9 -1 -2 -3 -4) ;-- --
  )
```

ну а далее я просто максимально тупым образом 
сделал селекторы кейсов и их хендлеры

после чего засунул итоговую таблицу в единый cond

## 2.12
```racket
#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound i)
  (min (car i) (cdr i)))

(define (upper-bound i)
  (max (car i) (cdr i)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (define (pos? x) (<= 0 x))
  
  (define x- (lower-bound x))
  (define x+ (upper-bound x))
  (define y- (lower-bound y))
  (define y+ (upper-bound y))
  
  (define (signs? a b c d)
    (and (eq? a (pos? x-))
         (eq? b (pos? x+))
         (eq? c (pos? y-))
         (eq? d (pos? y+))))
  
  (define (++++?) (signs? #t #t #t #t))
  (define (-+++?) (signs? #f #t #t #t))
  (define (--++?) (signs? #f #f #t #t))

  (define (++-+?) (signs? #t #t #f #t))
  (define (-+-+?) (signs? #f #t #f #t))
  (define (---+?) (signs? #f #f #f #t))

  (define (++--?) (signs? #t #t #f #f))
  (define (-+--?) (signs? #f #t #f #f))
  (define (----?) (signs? #f #f #f #f))

  (define (++++) 
    (make-interval (* x- y-)  
                   (* x+ y+)))
  (define (-+++) 
    (make-interval (* x- y+)
                   (* x+ y+)))
  (define (--++) 
    (make-interval (* x- y+) 
                   (* x+ y-)))

  (define (++-+) 
    (make-interval (* y- x+)
                   (* y+ x+)))
  (define (-+-+) 
    (make-interval (min (* x- y+) (* x+ y-)) 
                   (max (* x- y-) (* x+ y+))))
  (define (---+) 
    (make-interval (* x- y+) 
                   (* x- y-)))

  (define (++--) 
    (make-interval (* y- x+) 
                   (* y+ x-)))
  (define (-+--) 
    (make-interval (* y- x+) 
                   (* y- x-)))
  (define (----) 
    (make-interval (* x+ y+) 
                   (* x- y-)))

  (cond
    ((++++?) (++++))
    ((-+++?) (-+++))
    ((--++?) (--++))
                   
    ((++-+?) (++-+))
    ((-+-+?) (-+-+))
    ((---+?) (---+))
                   
    ((++--?) (++--))
    ((-+--?) (-+--))
    ((----?) (----))))

(define (interval->string x)
  (string-append "["
                 (number->string (lower-bound x))
                 ", "
                 (number->string (upper-bound x))
                 "]"))

(define (div-interval x y)
  (define (revert t)
    (if (not (= 0 t))
      (/ 1.0 t)
      (error (string-append "cannot divide by interval: " (interval->string y)))))
  (mul-interval x 
                (make-interval 
                 (revert (upper-bound y)) 
                 (revert (lower-bound y)))))

(define (sub-interval x y)
  (add-interval x
                (make-interval
                  (* -1 (upper-bound y))
                  (* -1 (lower-bound y)))))

(define (width x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

(define (print-interval x)
  (display (interval->string x))
  (newline))

(define (eq-interval? a b)
  (and (= (lower-bound a) (lower-bound b))
       (= (upper-bound a) (upper-bound b))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (make-center-percent c p)
  (make-center-width c
                     (/ (* c p) 
                        100.0)))

(define (percent i)
  (* 100.0 (/ (width i)
              (center i))))
```

## 2.13

давайте рассмотрим случай ++++
представим два промежутка в терминах центра и процентов
```
[c(1 - p), c(1 + p)] * [e(1 - k), e(1 + k)] = [ce(1-p)(1-k), ce(1 + p)(1 + k)]

    (1 + p)(1 + k) - (1 - p)(1 - k)
f = -------------------------------
    (1 + p)(1 + k) + (1 - p)(1 - k)

    1 + p + k + pk - 1 + k + p - pk
f = -------------------------------
    1 + p + k + pk + 1 - k - p + pk

     p + k       
f = -------
    1 + pk   
```

теперь вспомним, что p, k достаточно малы
а если более конкретно, давайте проанализируем ассимптотику f при ```|(p, k)| -> 0```

ну заметим, что ```1/(1 + pk) = 1 + o(1)```

следовательно ```f = (p + k) * (1 + o(1)) = p + k + o(p + k)```
ну или иначе говоря при малых значениях p, k

f ≈ p + k
                                                                                 
аналогичные формулы видимо для всех остальных случаев
но мы в это углубляться не будет

## 2.14
```
|center A|percent A|center B|percent B|percent A/A         |percent A/B       |
|--------|---------|--------|---------|--------------------|------------------|
|1.0     |5.0      |2.0     |2.0      |9.975062344139651   |6.993006993006996 |
|1.0     |0.1      |2.0     |1.0      |0.1999998000001888  |1.0999890001099975|
|15.0    |0.01     |30.0    |11.0     |0.019999999800004473|11.009878891332185|
|135.0   |4.0      |51.0    |3.0      |7.987220447284356   |6.9916100679185025|
|135.0   |4.0      |51.0    |4.0      |7.987220447284356   |7.987220447284351 |
|135.0   |8.0      |51.0    |8.0      |15.898251192368832  |15.898251192368843|
|135.0   |16.0     |51.0    |16.0     |31.201248049921993  |31.201248049921986|
```

по таблице можно заметить, что происходит примерно 
то, что мы наблюдали в 2.13 при умножении (проценты складываются)

в нашем случае происходит конечно деление, но если подумать
в вопросах процентов отличаться оно от умножения будет не очень сильно

какие это импликации в общем-то даёт?
а такие, что алгебраическое выражение равное 1, 1 +- 0% совсем этой единицей не является

и процент на самом деле еще и увеличивается

## 2.15
да, она действительно права
потому что если каждая независимая переменная появляется ровно один раз

то каждый раз вычисляется span некоторой непрерывной функции на компакте

ну вот рассмотрим некое выражение:

expr_1 ^ expr_2,

где expr_1 некое подвыражение, expr_2 некое подвыражение и ^ некая операция

так как expr_1 и expr_2 не содержат общих переменных
то множество значений функции I(expr_1 ^ expr_2) соответствует 
множеству {x ^ y | x ∈ I(expr_1), y ∈ I(expr_2)}

а это как раз наша интервальная арифметика

в том же случае, когда expr_1 и expr_2 зависят от некоторого общего параметра
то далеко не факт, что возможна любая комбинация x ^ y, потому что принятие expr_1 значения x может ограничивать множество значений, которое принимает expr_2

и следовательно вычисление таким наивным способом будет давать результат гораздо шире, чем хотелось бы

## 2.16

ну по большей части я уже обозначил проблему в 2.15
можно ли сделать программу, которая уже с точностью отвечает на подобные запросы?

ну... смотрите
финальное выражение — это некоторая дробно-рациональная функция нескольких переменных
заданная на многомерном параллепипеде

```
                             p(a_1, a_2, a_3, ..., a_n) 
f(a_1, a_2, a_3, ..., a_n) = --------------------------
                             q(a_1, a_2, a_3, ..., a_n) 
```

это функция задана на компакте
достигает своего минимума и максимума

и если наш пакет интервальной арифметики умеет как-то с этим работать
то он по сути решает задачу нахождения максимума/минимума подобной функции

в принципе это на самом деле даже возможно
но понятно, что масштаб проблемы достаточно серьезный

найти частные производные f, потом найти все вещественные корни 
на ребёрах многомерного параллелипипеда найти экстремумы
взять все вершины многомерного параллелиппипеда

и посчитать от этого всего максимум и минимум

как только мы умеем в такое
то мы достаточно легко реализуем пакет подобной интервальной арифметики

ведь теперь достаточно взять ту же идею:
если выражение разбивается на независимые компоненты
то давайте посчитаем по-отдельности и сделаем операцию на интервалах

если нет, то запускаем вот этот громоздкий дикий процесс
