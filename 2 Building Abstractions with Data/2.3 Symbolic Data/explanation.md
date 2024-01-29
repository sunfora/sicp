# 2.3.1 Quotation

## 2.53
```
(a b c)
((george))
((y1 y2))
(y1 y2)
#f
#f
(red shoes blue socks)       
```

## 2.54

```racket
#lang sicp

(define (atomic? x)
  (not (pair? x)))

(define (equal? x y)
  (or
    (and (atomic? x) (atomic? y) 
         (eq? x y))
    (and (pair? x) (pair? y)
         (equal? (car x) (car y))
         (equal? (cdr x) (cdr y)))))
```

Вообще говоря предикаты в лиспах проще всего строить с помощью ```or``` ```and``` и прочих конструкций. 

## 2.55

```
> (car (quote (quote abracadabra)))
quote
```

Ну потому что если в REPL написать ```(quote abcb)```, то мы получим тот же результат, что и при наборе ```'abcb```. Ну короче ```'``` это синтаксический сахар для ```quote```. Ваще говоря за этим поведение обычно стоит всякая гадость вроде READER MACRO. В кратце это такой макрос который пробегается перед компиляцией программы и делает всякие синтаксические трансформации. Например превращает ```'expr``` в ```(quote expr)```. Чуть позже мы возможно узнаем что на самом деле всё это может быть имплементировано в рамках самой scheme. Но я не буду особо много гадать на эту тему.

## 2.56

```racket
#lang sicp

(define variable?           
  ; Is e a variable?
  symbol?)

(define (same-variable? v1 v2) 
  ;Are v1 and v2 the same variable?
  (and (variable? v1) (variable? v2)
       (eq? v1 v2)))

(define (sum? e)
  ;Is e a sum?
  (and (pair? e) 
       (eq? '+ (car e))))

(define addend              
  ;Addend of the sum e.
  cadr)

(define augend              
  ;Augend of the sum e.
  caddr)

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  ;Construct the sum of a1 and a2.
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (product? e)           
  ;Is e a product?
  (and (pair? e) 
       (eq? '* (car e))))

(define multiplier          
  ;Multiplier of the product e.
  cadr)

(define multiplicand        
  ;Multiplicand of the product e.
  caddr)

(define (make-product m1 m2)   
  ;Construct the product of m1 and m2.
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

(define (exponentiation? e)           
  ;Is e an exponentiation? 
  (and (pair? e) 
       (eq? '** (car e))))

(define base 
  ;Base of the exponentiation e.
  cadr)

(define exponent 
  ;exponent of the exponentiation e.
  caddr)

(define (make-exponentiation b p)   
  ;Construct the exponentiation of b and p.
  (if (not (number? p))
    (error "non-number exponents are not supported"))
  (cond ((number? b) (expt b p))
        ((=number? p 1) b)
        ((=number? p 0) 1)
        (else (list '** b p))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (let ((n (exponent exp))
               (u (base exp)))
           (make-product 
             (make-product n 
               (make-exponentiation u (dec n)))
             (deriv u var))))
        (else (error "unknown expression 
                      type: DERIV" exp))))
```

## 2.57

```racket
#lang sicp

(define variable?           
  ; Is e a variable?
  symbol?)

(define (same-variable? v1 v2) 
  ;Are v1 and v2 the same variable?
  (and (variable? v1) (variable? v2)
       (eq? v1 v2)))

(define (add-operand operand operation)
  ; Adds something before operation
  ; For exampele (add-operand 0 '(foo 1 2))
  ; Yields '(foo 0 1 2)
  (cons (car operation)
        (cons operand
              (cdr operation))))

(define (sum? e)
  ;Is e a sum?
  (and (pair? e) 
       (eq? '+ (car e))))

(define addend              
  ;Addend of the sum e.
  cadr)

(define (augend e)
  ;Augend of the sum e.
  (if (null? (cdddr e))
      (caddr e)
      (cons '+ (cddr e))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))


(define (make-sum a1 a2)
  ;Construct the sum of a1 and a2.
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        ((sum? a1)
         (make-sum
           (addend a1)
           (make-sum (augend a1) a2)))
        ((sum? a2)
         (add-operand a1 a2))
        (else (list '+ a1 a2))))

(define (product? e)           
  ;Is e a product?
  (and (pair? e) 
       (eq? '* (car e))))

(define multiplier          
  ;Multiplier of the product e.
  cadr)

(define multiplicand        
  ;Multiplicand of the product e.
  augend)

(define (make-product m1 m2)   
  ;Construct the product of m1 and m2.
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        ((product? m1)
         (make-product
           (multiplier m1)
           (make-product (multiplicand m1) 
                         m2)))
        ((product? m2)
         (add-operand m1 m2))
        (else (list '* m1 m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

```

Итак, какая тут идея?
Давайте суммы хранить в форме ```(+ expr1 expr2 expr3 ... exprn)``` 
Как реализовать конструктор, чтобы он из одной или двух сумм собирал подобную, мы обсудим чуть позже.

Давайте подумаем как работают теперь наши селекторы:
 - Очевидно поведение ```addend``` поменяться не должно.
   ```
   > (addend '(+ 1 x y))
   1
   ```
 - А вот поведение ```augend``` поменяется.
   ```
   > (augend '(+ x y z))
   '(+ x y)
   > (augend '(+ x y)
   'y
   ```

Как это реализовать думаю вопросов не возникает.

Прежде чем мы перейдём к конструкторам, нам потребуется дополнительный примитив.
Мы должны научиться вносить внутрь суммы операнд.

Для этого определим ```add-operand```, с следующим поведением:
```
> (add-operand 'x '(foo y))
'(foo x y)
```

И теперь, у конструктора есть два случая:
 - Первый аргумент является суммой, тогда мы должны собрать сумму два раза:

   Первый раз из ```augend``` и ```exp```, а второй раз из ```addend``` и результата.
   ```
   (make-sum (+ 1 2 3) exp) -> (make-sum 1 (make-sum '(+ 2 3) exp))
   ```
   
   По сути мы делегируем разрешение этой ситуации в будущее второму варианту.

 - Первый аргумент суммой не является, а второй является.
   
   Ну тогда нам достаточно занести операнд с помощью ```add-operand``` в сумму.

Такие операции (в таком порядке), дадут нам следующее ожидаемое поведение: 
```
> (make-sum '(+ x y z) '(+ a b c))
(+ x y z a b c)
```

Аналогичная история для ```product```.

## 2.58

### 2.58.1
```racket
#lang sicp

(define variable?           
  ; Is e a variable?
  symbol?)

(define (same-variable? v1 v2) 
  ;Are v1 and v2 the same variable?
  (and (variable? v1) (variable? v2)
       (eq? v1 v2)))


(define (sum? e)
  ;Is e a sum?
  (and (pair? e) 
       (eq? '+ (cadr e))))

(define addend              
  ;Addend of the sum e.
  car)

(define augend              
  ;Augend of the sum e.
  caddr)

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  ;Construct the sum of a1 and a2.
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (product? e)           
  ;Is e a product?
  (and (pair? e) 
       (eq? '* (cadr e))))

(define multiplier          
  ;Multiplier of the product e.
  car)

(define multiplicand        
  ;Multiplicand of the product e.
  caddr)

(define (make-product m1 m2)   
  ;Construct the product of m1 and m2.
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list m1 '* m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))))
```

Так как конструкторы и селекторы дают хорошую изоляцию структуры данных от происходящего внутри```deriv```, достаточно поменять их. Что мы тривиальным образом и сделали.

Для этого достаточно было подменить порядок выражений в конструкторах и селекторах.

Нечто похожее в общем-то происходило в прошлом задании, которое требовало подменить конструкторы так, чтобы у нас получилось дифференцирование многих членов сразу. 

### 2.58.2

```racket
#lang sicp

(define variable?           
  ; Is e a variable?
  symbol?)

(define (same-variable? v1 v2) 
  ;Are v1 and v2 the same variable?
  (and (variable? v1) (variable? v2)
       (eq? v1 v2)))

(define (^list p?)
  (lambda (e)
    (if (p? e)
      e
      (list e))))

(define (foldr start op lst)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter start lst))

(define (or-pred . preds)
  (lambda (e)
    (foldr #f 
           (lambda (result p?)
             (or result
                 (p? e)))
           preds)))

(define (cut-last lst)
  (define (iter without rest)
    (if (null? (cdr rest))
      (list (reverse without) (car rest))
      (iter (cons (car rest) without)
            (cdr rest))))
  (iter nil lst))

(define (join-by . rst)
  (let ((x (cut-last rst)))
    (let ((preds (car x))
          (elems (cadr x)))
      (apply append
             (map (^list (apply or-pred preds)) 
                  elems)))))

(define (complement-memq obj expr)
  (define (iter result expr)
    (cond ((null? expr) #f) 
          ((eq? obj (car expr)) result)
          (else (iter (cons (car expr)
                            result)
                      (cdr expr)))))
  (reverse (iter nil expr)))

(define (car-if-1 lst)
  (if (null? (cdr lst))
    (car lst)
    lst))

(define (first-operand operation)
  (lambda (expression)
    (car-if-1 (complement-memq operation
                               expression))))

(define (second-operand operation)
  (lambda (expression)
    (car-if-1 (cdr (memq operation
                         expression)))))

(define (has-operation? operation expression)
  (if (and (pair? expression)
           (memq operation expression))
    #t #f))

(define (sum? e)
  ;Is e a sum?
  (has-operation? '+ e))

(define addend             
  ;Addend of the sum e.
  (first-operand '+))

(define augend              
  ;Augend of the sum e.
  (second-operand '+))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  ;Construct the sum of a1 and a2.
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (join-by sum? 
                       product? 
                       (list a1 '+ a2)))))

(define (product? e)           
  ;Is e a product?
  (and (not (sum? e))
       (has-operation? '* e)))

(define multiplier          
  ;Multiplier of the product e.
  (first-operand '*))

(define multiplicand 
  ;Multiplicand of the product e.
  (second-operand '*))

(define (make-product m1 m2)   
  ;Construct the product of m1 and m2.
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (join-by product? 
                       (list m1 '* m2)))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))))
```

Эта часть действительно сложнее.

Давайте по порядку, заведем примитивы, чтобы с этим разобраться:
 - Примитив ```complement-memq```, буквально дополнение ```memq```
   ```
   > (complement-memq 'c '(a b c d))
   '(a b)
   ```
 - Примитив ```car-if-1```
   ```
   > (car-if-1 '(x y))
   '(x y)
   > (car-if-1 '(x))
   x
   ```

С помощью них уже можно что-то задать. 

Например определить, что нечто является суммой достаточно просто: должен присутствовать знак ```'+``` в списке. В самом деле так как сумма является операцией с наименьшим приоритетом, то ничего больше проверять не надо.

Чтобы сделать ```addend```, нам потребуется уже 2 примитива: ```car-if-1``` и ```complement-memq```, в самом деле, так как мы операция наименьшего приоритета, то всё, что стояло до первого знака ```'+```, относиться не к нам, а к какой-то другой операции. А если операцией оно не является, то что же делать? Ну достаточно в этом случае достать элемент с помощью ```car-if-1```.

Ситуация с ```augend``` симметричная: car-if-1, cdr, memq.

Вообще говоря если на это посмотреть, то легко увидеть, что в общем-то аналогичная ситуация будет для ```product```. Ведь когда мы знаем, что нечто является ```product```, мы точно можем сказать, что умножение здесь будет операцией наименьшего приоритета. Что вселяет мысли о том, что в теории можно всё это задать каким-то общим образом.

Ну действительно, давайте зададим две операции: ```first-operand``` и ```second-operand```. Которые в зависимости от знака операции, будут нам выдавать значения. Как они реализованы, можно посмотреть выше.

Соответственно селекторы для ```sum```, ```product``` мы реализовали.

Давайте теперь сделаем конструкторы, в чём основная сложность?

Самая большая сложность в том, чтобы как-то легко конкатенировать операции в зависимости от того, являются ли они операциями определенного вида.

Хочется примерно такого поведения: ```join '((a + b + c) + (c d e)) -> '(a + b + c + c d e)```.

Чтобы конкатенировать 2 списка, у нас есть замечательная ```append```. 
Превратим отдельные элементы в списки в зависимости от того, сработает ли предикат или нет.

Ну например хочется такого поведения:
```
> (map (^list product?) '((a * b) c (d + e))
'((a * b) (c) ((d + e)))
```

Добавляя к этому ```apply append```, мы получаем то, что будет зваться ```join-by```.

```racket
(define (join-by pred lst)
  (apply append (map (^list pred) lst)))
```

```
> (join-by product? '((a * b) c (d + e)))
'(a * b c (d + e))
```

Дальше при реализации ```sum``` у нас возникнет проблема: необходимо несколько предикатов и какой-то из них должен сработать. 

Ну то есть хочется:
```(join-by (or sum? product?) lst)```

В принципе можно так и сделать, но я решил модифицировать ```join-by```, чтобы он полностью собирал ```or-pred``` и получался вызов вида:

```
> (join-by sum? 
           product?
           '((a * b) + (c * d) + (e + f)))
(a * b + c * d + e + f)
```

Ну а отсюда уже достаточно легко определить конструкторы, надо просто финальное выржение подменить на соответствующие вызовы ```join-by```.

## 2.59

