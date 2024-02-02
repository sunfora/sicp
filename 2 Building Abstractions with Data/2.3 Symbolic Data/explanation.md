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

# Representing sets

## 2.59

```racket
#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 set2)))
        (else (intersection-set (cdr set1)
                                set2))))

(define (union-set set1 set2)
  (if (null? set2)
    set1
    (union-set (adjoin-set (car set2)
                           set1)
               (cdr set2))))
```

Ну простая рекурсивная стратегия: если второй пуст, то точно возвращаем первый.
Во всех остальных случаях сводим задачу к тому, чтобы зааджойнить первый сет ко второму.

## 2.60

```racket
#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (if (or (null? set1) 
          (null? set2))
    '()
    (let ((intersected (intersection-set 
                        (cdr set1)
                        set2))
          (elem (car set1)))
      (if (and (element-of-set? elem set2)
               (not (element-of-set? elem intersected)))
        (cons elem intersected)
        intersected))))

(define (drop-duplicates set)
  (intersection-set set set))

(define (union-set set1 set2)
  (append set1 set2))
```

У нас практически всё упростилось, потому что теперь объединение это простая конкатенация. Единственное что не поменялось, так это пересечение. Ну и так как там всё равно n^2, я решил дропнуть заодно дубликаты в пересечении. За что бесплатно получил ```drop-duplicates``` метод, который тоже за n^2 работает.

Когда подобную реализацию стоит использовать? Ну в том случае если у нас не очень частые пересечения. Либо данные изначально мы знаем, что достаточно редко повторяются. Казалось бы в чём смысл, а в том, что если мы знаем, что данные достаточно рандомные, то смысл нам избегать каждый дубликат, их будет не очень много.

Из минусов, у нас большие затраты по памяти, объедини 3 одинаковых сета и получишь бессмысленную трату памяти в 3 раза большую, чем у предыдущего варианта. Опять же поэтому такой вариант стоит предпочесть для в достаточной мере рандомных данных.

## 2.61

```racket
#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
```

Ну тут всё до жути похоже на ```element-of-set```, поэтому как-то специально рассматривать я не буду. "В среднем", ну то есть если у нас рандомный инпут, ```n/2``` получается операций.

## 2.62

```racket
#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set 
                          set1 
                          (cdr set2)))))))

(define (union-set set1 set2)
  (if (or (null? set1) (null? set2))
      (append set1 set2)
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (union-set 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2)
               (cons x1 (union-set 
                          (cdr set1) 
                          set2))) 
              ((> x1 x2)
               (cons x2 (union-set 
                          set1 
                          (cdr set2))))))))
```

Здесь уже мы видим симметрию с ```intersect```.
Ну почему O(n), потому что каждый раз вызывается ровно одна рекурсивная операция, при этом при обратном движении рекурсии просто добавляется в хвост операция константного времени исполнения.

## AVL деревья

Я решил немного ерундой позаниматься и сделать AVL дерево.
Книга нас об этом не просит, она как бы упоминает, что это возможно (деревья балансировать), но не более того.

Раньше я уже писал AVL дерево на плюсах (и получилось на самом деле не так душно как мне казалось).
Теперь вот попробую на scheme.

Сначала я предлагаю немного поменять ```adjoin-set```, на следующее:
```racket
(define (adjoin-set x set)
  (define alter AVL-balance)
  (define (adjoin-x set) (adjoin-set x set))
  (define (default)
    (cond ((null? set) (make-tree x '() '()))
          ((= x (entry set)) set)
          ((< x (entry set))
           ((on-left-branch adjoin-x) set))
          ((> x (entry set))
           ((on-right-branch adjoin-x) set))))
  (alter (default)))
```

А на место ```AVL-balance``` мы пока поставим заглушку такого вида:
```racket
(define AVL-balance idenity)
```

Ну, пока мы ничего не меняли, ничего и не должно поменяться. 
Тем не менее, мы хотим после каждой вставки в множество перебалансировать дерево.

### Что такое AVL дерево? 

И давайте я кратко вас ознакомлю с идеей AVL дерева. Ну суть очень простая:
Давайте дерево считать сбалансированным, если дети сбалансированы и высота детей отличается не более чем на 1.

```|(height left) - (height right)| < 1```

Почему дерево получается действительно сбалансированным? То есть его высота порядка O(log n), где n количество элементов в дереве.

Ну давайте это докажем: рассмотрим AVL-дерево высоты h. 
И давайте заведем N(h), которое будет числом, самого минимального AVL дерева такой высоты.

Ну условно для одной и той же высоты есть несколько вариантов AVL деревьев, ну например:

```
     4         4
    / \       / \
   3   5     3   5
  / \       / \   \
 1   2     1   2   6
```

Оба дерева валидные AVL деревья, у обоих одна и та же высота, но мы рассматриваем деревья с минимальным количеством элементов. В случае h = 3 это например N(3) = 4.

```
     3  
    / \ 
   2   4
  /     
 1      
```

У нашего дерева оба поддерева тоже AVL деревья, причем если наше имело высоту h, то поддеревья могут принимать два варианта: {h-1, h-1}, {h-2, h-1}.

В любом случае получаем следующее соотношение:
```
N(h) = 1 + min(N(h - 1) + N(h - 2), N(h - 1) + N(h - 1))
```

Интуитивно понятно, что N(h) вещь таки монотонная, поэтому давайте это проясним.
```
N(1) = 1
N(2) = 2
```

Предположим, что мы доказали до шага h. Но тогда
```
N(h) = 1 + N(h - 1) + N(h - 2)
```

И шаг N(h + 1) тривиально проверяется:
```
N(h + 1) - N(h) = N(h) - N(h - 2) > 0
```

Следовательно на самом деле N(h) можно выразить как:
```
N(h) = 1 + N(h - 1) + N(h - 2)
```

На самом деле уже отсюда можно поупражнятся в решении уравнений с линейной рекурсией.
Но я воспользуюсь [maxima](https://maxima.sourceforge.io/ru/index.html).

```maxima
load("solve_rec")$
rec: N[h] = 2 * N[h - 1] - N[h - 3];
solve_rec(rec, N[0] = 0, N[1] = 1, N[2] = 2, N[h]);
```

И получить результат:
```
N[h]=-((sqrt(5)-1)^h*(3*sqrt(5)-5)*2^(-h-1)*(-1)^h)/5+((sqrt(5)+1)^h*(3*sqrt(5)+5)*2^(-h-1))/5-1
N[h] = 1/10 ((5 - 3 sqrt(5)) (-ϕ)^-n + (5 + 3 sqrt(5)) ϕ^n - 10)
```

Который можно много раз попреобразовывать, чтобы получить:
```
N[h] + 1 = 1/10 ((5 - 3 sqrt(5)) (-ϕ)^-n + (5 + 3 sqrt(5)) ϕ^n)
N[h] + 1 =  -(-ϕ)^-2/sqrt(5) (-ϕ)^-n + ϕ^2/sqrt(5) ϕ^n
N[h] + 1 =  ( ϕ^(n+2) - (-ϕ)^-(n+2) ) / sqrt(5)
N[h] + 1 = Fib[n + 2]
N[h] = Fib[h + 2] - 1
```

Что достаточно забавный результат. 

Но давайте чуть проще это всё докажем, просто по мат-индукции:
```
N[0] = 0 = Fib[2] - 1 = 1 - 1 = 0
N[1] = 1 = Fib[3] - 1 = 2 - 1 = 1
N[2] = 2 = Fib[4] - 1 = 3 - 1 = 2

N[h + 1] = 1 + N[h] + N[h-1] 
         = 1 + Fib[h + 2] - 1 + Fib[h + 1] - 1 
         = Fib[h + 3] - 1
```

Который нас приводит к следующему выводу, пусть есть AVL-дерево с количеством узлов n и высотой h.
Но тогда 
```
N[h] < n
h =< log((n + 1) * sqrt(5), ϕ) - 2
```

Что конечно даёт O(log n) высоту.

Можно кстати написать сейчас соответствующий метод и протестировать его в будущем:
```racket
(define phi (/ (inc (sqrt 5)) 2))

(define (aproximate-height n)
  (- (inexact->exact 
       (round (log (* (+ 1 n) (sqrt 5)) phi))) 
     2))
```

### Вращения

```
     d             b   
    / \           / \ 
   b  [e]  <->  [a]  d
  / \               / \ 
[a] [c]           [c] [e]

   a < b < c < d < e
```

На деревьях поиска можно установить забавную операцию, которая сохраняет структуру дерева поиска.
И при этом любое дерево ```T_1``` может быть серией вращений преобразовано в дерево ```T_2```.

Это на самом деле легко доказывается, достаточно доказать, что любое дерево раскручивается вращениями в бамбук.
Давайте запишем серию этих вращений в виде какого-то списка в стиле:
```
[left | right] value
```

Вращая в обратном порядке, а именно при замене: 
```
left  vertex -> right (lower value)
right vertex -> left  (upper value)
```

Где ```(upper value)``` это следующее значение в множестве больше текущего, а ```(lower value)``` аналогично меньше текущего.

Останется развернуть список и мы получим уже список который бамбук превращает в дерево.

Это всё вытекает из свойств:
```
> (equal? (rotate-left (rotate-right tree)) tree)
true
> (equal? (entry (rotate-left tree)) (entry (right-branch tree)))
true
> (equal? (entry (rotate-right tree)) (entry (left-branch tree)))
true
```

Теперь "сериализуем" ```T_1``` и ``T_2```. Развернем по смыслу список ```T_2```, и сконкатенируем их.
У нас получился список операций который приводит ```T_1 -> бамбук -> T_2```.

Кстати говоря, размер списка это количество вращений необходимое для перевода одного дерева в другое.
Потому что в самом деле можно всегда развернуть список и получить преобразование ```T_2 -> T_1```.

Естественно подобный способ не единственный и на проверку оказывается достаточно сложно придумать какой-то полиномиальный алгоритм, который бы находил минимальный подобный список. Это открытая проблема в computer science.

> Determining the complexity of computing the rotation distance exactly without parameterization remains unsolved, and the best algorithms currently known for the problem run in exponential time. [wikipedia](https://en.wikipedia.org/wiki/Rotation_distance)

Давайте теперь реализуем вращения, сначала введем ряд вспомогашек, их суть как бы убрать постоянную необходимость пересоздавать идентичное дерево, в котором поменялась только правая или левая ветка.

```racket
(define (on-left-branch f)
  (lambda (tree)
    (make-tree (entry tree)
               (f (left-branch tree))
               (right-branch tree))))

(define (on-right-branch f)
  (lambda (tree)
    (make-tree (entry tree)
               (left-branch tree)
               (f (right-branch tree)))))

(define (replace-right-branch tree branch)
  ((on-right-branch (lambda (right) branch)) tree))

(define (replace-left-branch tree branch)
  ((on-left-branch (lambda (left) branch)) tree))
```

И теперь досаточно коротко можно записать вращение деревьев как:
```racket
(define (rotate-left tree)
  (replace-left-branch 
    (right-branch tree)
    ((on-right-branch left-branch) tree)))

(define (rotate-right tree)
  (replace-right-branch 
    (left-branch tree)
    ((on-left-branch right-branch) tree)))
```

### О балансировке AVL деревьев

Давайте ближе к делу.

Вот у нас было AVL дерево, мы что-то вставили и удалили. 
Как нам теперь отбалансировать результат?

Ну начнем с того, будем балансировать рекурсивно, поэтому мы ожидаем, что дети наши уже сбалансированы.

Тогда какие у нас могут варианты быть после вставки?

1. Дерево осталось сбалансированным. 
   Замечательно, ничего делать не надо.
2. Один из сыновей больше другого на 2.

Так как всё симметрично, давайте без уменьшения общности положим, что правый сын больше.

Теперь у нас снова два варианта:
1. Правый сын правого больше или равен левого.
2. Левый сын правого больше.

Первая картинка выглядит вот так:
```
1)    (h+3)           2)    (h+3)          
      /   \                 /   \         
    (h)   (h+2)           (h)  (h+2)     
          /  \                 /  \     
         /    \               /    \    
     (h+1)    (h+1)         (h)    (h+1)
```

Что в первом, что во втором случае, правое вращение избавит нас от проблемы.
```
1)      (h+3)           2)      (h+2)   
        /  \                    /  \    
     (h+2) (h+1)             (h+1) (h+1)
     /  \                    /  \       
    /    \                  /    \      
  (h)   (h+1)             (h)    (h)   
```

То есть это просто ```rotate-right```.

Немножко другая вторая картинка:
```
1)    (h+3)            (h+3)                            
      /   \            /   \                       (h+2)           
    (h)  (h+2)       (h)  (h+2)                    /    \          
         /  \   ==>       /   \       ==>         /      \                        
        /    \           /     \               (h+1)      (h+1)      
     (h+1)   (h)      (h|h-1) (h+1)           /    \      /   \    
     /    \                   /   \          /      \    /     \              
  (h|h-1) (h|h-1)          (h|h-1) (h)     (h)  (h|h-1) (h|h-1) (h)            
```

Тут прежде чем вращать всё дерево, надо повращать правого сына.
И вращать, как по картинке вы заметили, мы будем так, чтобы правая ветка правого сына стала тяжелее.

Чтобы потом мы её развернули как в первом случае правым вращением.

Давайте заведем парочку функциональных примтитивов, для бесточечного программирования.
И опишем алгос уже.
```racket
(define (foldr op start seq)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
  (iter start seq))

(define (fork p? f g)
  (lambda (x)
    (if (p? x) 
      (f x) (g x))))

; композиция функций в обратном порядке
(define (-> . funcs)
  (foldr (lambda (r f)
           (lambda (x)
             (f (r x))))
         identity
         funcs))
```

Нам надо как-то хранить сведения о высоте, для этого мы модифицируем конструктор, добавим пару селекторов и модифицируем существующие селекторы, чтобы они не мешались на случай null-ов.

```racket
(define (make-tree entry left right)
  (list entry left right 
        (inc (max (height left)
                  (height right)))))

(define (entry tree) (car tree))

(define (left-branch tree) 
  (if (null? tree) '() (cadr tree)))

(define (right-branch tree)
  (if (null? tree) '() (caddr tree)))

(define (height tree)
  (if (null? tree) 0 (cadddr tree)))

(define (balance-factor tree)
  (- (height (right-branch tree))
     (height (left-branch tree))))
```

И теперь мы можем приступать:
```racket
;; AVL-tree is considered balanced
;; if |(height right) - (height left)| < 1
(define (balanced? tree)
  (or (= -1 (balance-factor tree))
      (= 0 (balance-factor tree))
      (= 1 (balance-factor tree))))

;; apply left function if left is higher
;; or right function otherwise
(define (when-higher left-transform right-transform)
  (lambda (tree)
    ((if (>= (height (left-branch tree))
             (height (right-branch tree)))
       left-transform right-transform) tree)))

(define AVL-balance
  (fork balanced?
        identity
        (when-higher 
          (-> (on-left-branch 
                (when-higher 
                  identity rotate-left))
              rotate-right)
          (-> (on-right-branch 
                (when-higher 
                  rotate-right identity))
              rotate-left))))
```

Давайте еще чуть-чуть про удаление значений из дерева.
Ну на самом деле это не очень сложно, чтобы удалить вершину в самом верху и сохранить порядок, нам надо удалить минимум в правом поддереве (значения в котором больше удаляемой вершины) и засунуть её наверх. Балансировка аналогично происходит по ходу того как мы спускаемся ниже и ниже.

```racket
(define (leaf? set)
  (and (null? (left-branch set))
       (null? (right-branch set))))

(define (find-min set)
  (cond 
    ((null? set) '())
    ((null? (left-branch set))
     (entry set))
    (else (find-min (left-branch set)))))

(define (find-max set)
  (cond 
    ((null? set) '())
    ((null? (right-branch set))
     (entry set))
    (else (find-max (right-branch set)))))

(define (remove-head set)
  (cond ((leaf? set) '())
        ((null? (left-branch set)) 
         (right-branch set))
        ((null? (right-branch set))
         (left-branch set))
        (else
          (let ((m (find-min (right-branch set))))
            (make-tree m
                       (left-branch set)
                       (remove-set m (right-branch set)))))))

(define (remove-set x set)
  (define alter AVL-balance)
  (define (remove-x set)
    (remove-set x set))
  (define (default)
    (cond ((null? set) set)
          ((= x (entry set))
           (remove-head set))
          ((< x (entry set))
           ((on-left-branch remove-x) set))
          ((> x (entry set))
           ((on-right-branch remove-x) set))))
  (alter (default)))
```

### Как растёт высота AVL дерева на практике

```
n       min     actual  max
1       1       1       1
5       3       3       3
10      4       4       5
20      5       5       6
50      6       7       8
100     7       8       9
200     8       9       11
500     9       11      13
1000    10      12      14
2000    11      13      15
5000    13      14      17
10000   14      16      19
20000   15      17      20
50000   16      19      22
100000  17      20      24
200000  18      21      25
500000  19      23      27
1000000 20      24      28
```

за min взято идеально сбалансированное дерево
за max взята наша оценка высоты

### Полный листинг
```racket
#lang sicp

;==== general functional things ====

(define (foldr op start seq)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
  (iter start seq))

(define (fork p? f g)
  (lambda (x)
    (if (p? x) 
      (f x) (g x))))

(define (-> . funcs)
  (foldr (lambda (r f)
           (lambda (x)
             (f (r x))))
         identity
         funcs))

;==== selectors & constructors ====
(define (make-tree entry left right)
  (list entry left right 
        (inc (max (height left)
                  (height right)))))

(define (entry tree) (car tree))

(define (left-branch tree) 
  (if (null? tree) '() (cadr tree)))

(define (right-branch tree)
  (if (null? tree) '() (caddr tree)))

(define (height tree)
  (if (null? tree) 0 (cadddr tree)))

(define (balance-factor tree)
  (- (height (right-branch tree))
     (height (left-branch tree))))

;==== various tree-operations shorthands ====

(define (on-left-branch f)
  (lambda (tree)
    (make-tree (entry tree)
               (f (left-branch tree))
               (right-branch tree))))

(define (on-right-branch f)
  (lambda (tree)
    (make-tree (entry tree)
               (left-branch tree)
               (f (right-branch tree)))))

(define (replace-right-branch tree branch)
  ((on-right-branch (lambda (right) branch)) tree))

(define (replace-left-branch tree branch)
  ((on-left-branch (lambda (left) branch)) tree))

;==== balancing ====

(define (rotate-left tree)
  (replace-left-branch 
    (right-branch tree)
    ((on-right-branch left-branch) tree)))

(define (rotate-right tree)
  (replace-right-branch 
    (left-branch tree)
    ((on-left-branch right-branch) tree)))

;; AVL-tree is considered balanced
;; if |(height right) - (height left)| < 1
(define (balanced? tree)
  (or (= -1 (balance-factor tree))
      (= 0 (balance-factor tree))
      (= 1 (balance-factor tree))))

;; apply left function if left is higher
;; or right function otherwise
(define (when-higher left-transform right-transform)
  (lambda (tree)
    ((if (>= (height (left-branch tree))
             (height (right-branch tree)))
       left-transform right-transform) tree)))


(define AVL-balance
  (fork balanced?
        identity
        (when-higher 
          (-> (on-left-branch 
                (when-higher 
                  identity rotate-left))
              rotate-right)
          (-> (on-right-branch 
                (when-higher 
                  rotate-right identity))
              rotate-left))))

;==== Set operations ====

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

(define (adjoin-set x set)
  (define alter AVL-balance)
  (define (adjoin-x set) (adjoin-set x set))
  (define (default)
    (cond ((null? set) (make-tree x '() '()))
          ((= x (entry set)) set)
          ((< x (entry set))
           ((on-left-branch adjoin-x) set))
          ((> x (entry set))
           ((on-right-branch adjoin-x) set))))
  (alter (default)))

(define (leaf? set)
  (and (null? (left-branch set))
       (null? (right-branch set))))

(define (find-min set)
  (cond 
    ((null? set) '())
    ((null? (left-branch set))
     (entry set))
    (else (find-min (left-branch set)))))

(define (find-max set)
  (cond 
    ((null? set) '())
    ((null? (right-branch set))
     (entry set))
    (else (find-max (right-branch set)))))

(define (remove-head set)
  (cond ((leaf? set) '())
        ((null? (left-branch set)) 
         (right-branch set))
        ((null? (right-branch set))
         (left-branch set))
        (else
          (let ((m (find-min (right-branch set))))
            (make-tree m
                       (left-branch set)
                       (remove-set m (right-branch set)))))))

(define (remove-set x set)
  (define alter AVL-balance)
  (define (remove-x set)
    (remove-set x set))
  (define (default)
    (cond ((null? set) set)
          ((= x (entry set))
           (remove-head set))
          ((< x (entry set))
           ((on-left-branch remove-x) set))
          ((> x (entry set))
           ((on-right-branch remove-x) set))))
  (alter (default)))

;==== debuging ====

(define (display-tree tree)
  (define (pad n)
    (make-string n (string-ref " " 0)))
  (define (display-padded n str)
    (display (string-append (pad n)
                            str
                            "\n")))
  (define (display-tree-padded level tree)
    (if (null? tree)
      (display-padded level "x")
      (begin 
        (display-tree-padded 
          (+ level 4) 
          (left-branch tree))
        (display-padded 
          level 
          (number->string (entry tree)))
        (display-tree-padded 
          (+ level 4) 
          (right-branch tree)))))
  (display-tree-padded 0 tree))

;==== examining height ====               
(#%require (only racket require
                        prefix-in
                        only-in))
(require (prefix-in rkt: 
                    (only-in racket
                             shuffle
                             in-inclusive-range
                             sequence->list)))
(require (prefix-in rkt: 
                    (only-in racket/mpair
                             mlist->list
                             list->mlist)))

(define (convert fn)
  (-> rkt:mlist->list
      fn
      rkt:list->mlist))

(define shuffle (convert rkt:shuffle)) 

(define (nums-seq n)
  (rkt:in-inclusive-range 1 n))

(define nums
  (-> nums-seq 
      rkt:sequence->list
      rkt:list->mlist))

(define (adjoin-values tree vals)
  (foldr (lambda (tree x)
           (adjoin-set x tree))
         tree
         vals))

(define (remove-values tree vals)
  (foldr (lambda (tree x)
           (remove-set x tree))
         tree
         vals))

(define phi (/ (inc (sqrt 5)) 2))

(define (max-height n)
  (- (inexact->exact 
       (round (log (* (+ 1 n) (sqrt 5)) phi))) 
     2))

(define (actual-height n)
  (height (adjoin-values '() (shuffle (nums n)))))

(define (min-height n)
  (inexact->exact (ceiling (log (inc n) 2))))

(define (test n)
  (string-append (number->string n) "\t"
                 (number->string (min-height n)) "\t"
                 (number->string (actual-height n)) "\t"
                 (number->string (max-height n)) "\n"))

(display (string-append "n\tmin\tactual\tmax\n"
                        (test 1)
                        (test 5)
                        (test 10)
                        (test 20)
                        (test 50)
                        (test 100)
                        (test 200)
                        (test 500)
                        (test 1000)
                        (test 2000)
                        (test 5000)
                        (test 10000)
                        (test 20000)
                        (test 50000)
                        (test 100000)
                        (test 200000)
                        (test 500000)
                        (test 1000000)))
```

## 2.63

