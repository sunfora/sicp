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

# 2.3.2 Symbolic Differentiation

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

# 2.3.3 Representing sets

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

## Балансирование деревяшек (AVL деревья)

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

### Что такое AVL дерево? 

Давайте дерево считать сбалансированным, если дети сбалансированы и высота детей отличается не более чем на 1.

```|(height left) - (height right)| < 1```

Почему дерево получается действительно сбалансированным? То есть его высота порядка O(log n), где n количество элементов в дереве.

Рассмотрим некое AVL-дерево высоты h.

Давайте заведем характеристику:
```N(h) — число вершин самого минимального AVL дерева высоты h```

Ну условно для одной и той же высоты есть несколько вариантов AVL деревьев, например:

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

Что на самом деле почти что числа Фибоначи. Ну и действительно это они:
```
# база
N[0] = 0 = Fib[0 + 2] - 1 = 1 - 1 = 0
N[1] = 1 = Fib[1 + 2] - 1 = 2 - 1 = 1

# предположение
N[h] = Fib[h + 2] - 1

N[h + 1] = 1 + N[h] + N[h-1] 
         = 1 + Fib[h + 2] - 1 + Fib[h + 1] - 1 
         = Fib[h + 1 + 2] - 1
```

Что достаточно забавно и очень кстати для нас. 
Потому что числа Фибоначи весьма экспоненциальная гадость. 

Более того мы уже ранее выводили для них оценку (см разбор задания 1.13).

Пусть есть AVL-дерево с количеством узлов n и высотой h, тогда:
```
N[h] < n
h =< log((n + 1) * sqrt(5), ϕ) - 2
```

Что конечно даёт O(log n) асимптотику на высоту.
Так как все операции перебалансировки будут иметь константное время выполнения, ```insert```, ```remove``` будут работать за O(log n).

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

Теперь "сериализуем" ```T_1``` и ```T_2```. Развернем по смыслу список ```T_2```, и сконкатенируем их.
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

```
#lang sicp

(define (entry tree) (cadr tree))
(define (left-branch tree) (car tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list left entry right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define trees-2.16
  (list '(((() 1 ()) 3 (() 5 ())) 7 (() 9 (() 11 ())))
        '((() 1 ()) 3 ((() 5 ()) 7 (() 9 (() 11 ()))))
        '(((() 1 ()) 3 ()) 5 ((() 7 ()) 9 (() 11 ())))))

(define (show fn)
  (display fn) 
  (newline)
  (for-each (lambda (lst)
              (display lst)
              (newline))
            (map fn trees-2.16)))

(show tree->list-1)
(show tree->list-2)

(define (loop n f)
  (if (zero? n)
    '()
    (begin (f)
           (loop (dec n) f))))

(define (timed fn . args)
  (define times 10.0)
  (let ((t (runtime)))
    (loop times (lambda () (apply fn args)))
    (/ (- (runtime) t)
       times)))

(define (left-bamboo n)
  (if (zero? n)
    '()
    (make-tree n
               (left-bamboo (dec n))
               '())))

(define (bamboo-test fn n)
  (timed fn (left-bamboo n)))

(define (test n)
  (let ((t-1 (bamboo-test tree->list-1 n))
        (t-2 (bamboo-test tree->list-2 n))) 
    (string-append
      (number->string n)   "\t"
      (number->string t-2) "\t"
      (number->string t-1) "\t"
      (number->string (round (/ t-1 t-2))))))

(define (make-table a b step)
  (define (iter i)
    (if (< i b)
      (string-append (test i) "\n"
                     (iter (+ step i)))
      ""))
  (iter a))

(display 
  (string-append
    "size\tsecond\tfirst\tratio\n"
    (make-table 10   100   10 )     
    (make-table 100  1000  100)     
    (make-table 1000 10000 1000)))
```

Я немного поменял очередь в конструкторе, чтобы проще было записывать деревья.
Легко доказать, что "сериализация" дерева в таком виде, всегда выглядит как отсортированная последовательность чисел. Так как это совсем просто, не будем в это углубляться.

Собственно запуская код получаем:
```
#<procedure:tree->list-1>
(1 3 5 7 9 11)
(1 3 5 7 9 11)
(1 3 5 7 9 11)
#<procedure:tree->list-2>
(1 3 5 7 9 11)
(1 3 5 7 9 11)
(1 3 5 7 9 11)
```

Что касается вопросов:
1. Да, для любого списка мы получим эквивалентные результаты.
   Формально можно так доказать:

   ```tree->list-1``` возвращает в отсортированном порядке все элементы дерева
   Это справедливо для пустого дерева и для листа, а всё остальное следует из индукции по структуре.

   Аналогично ```tree->list-2``` возвращает в отсортированном порядке все элементы дерева.
   
   Опять же, справедливо, для пустого дерева и листьев, что ```copy-to-list``` добавляет вершины в отсортированном порядке. Ну и индукцией по структуре легко видеть, что действительно справедливо утверждение и для большего дерева.

   Ну а так как мы приписываем теперь всё дерево к пустому списку, то мы получаем список всех вершин в отсортированном порядке.

   Отсюда следует что для всех деревьев функции возвращают один и тот же результат.

2. Ну, it depends. Мы append не писали, мало ли. Согласно тому, что я могу прочитать [здесь](https://groups.csail.mit.edu/mac/ftpdir/scheme-reports/r5rs-html/r5rs_8.html), ```append``` работать должен за размер первого аргумента, если не происходит какой-то магии. Поэтому в худшем случае мы не только собираем список, но и каждый раз полностью пробегаемся по левой ветке. Что даёт n^2 в случае левого бамбука. Давайте даже тайм тест сделаем.

```
size   second      first    ratio   
  10      0.3        1.5      5.0
  20      0.5        2.3      5.0
  30      0.8        4.5      6.0
  40      1.0        7.2      7.0
  50      1.3       11.3      9.0
  60      2.0       16.3      8.0
  70      2.7       20.9      8.0
  80      2.0       28.7     14.0
  90      2.1       37.2     18.0
 100      2.8      117.6     42.0
 200      4.6      147.5     32.0
 300      6.7      420.2     63.0
 400      9.1      588.0     65.0
 500     16.5     1292.2     78.0
 600     12.7     1707.8    134.0
 700     15.5     1841.2    119.0
 800     17.5     2418.9    138.0
 900     20.2     3155.0    156.0
1000     22.7     3913.4    172.0
2000     44.4    15936.0    359.0
3000     72.3    37144.0    514.0
4000     86.1    66359.2    771.0
5000    136.3   105980.0    778.0
6000    191.5   158855.0    830.0
7000    214.7   213876.7    996.0
8000    258.6   285184.9   1103.0
9000    197.0   456217.9   2316.0
```

## 2.64

```racket
#lang sicp

(define (entry tree) (cadr tree))
(define (left-branch tree) (car tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list left entry right))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size      (quotient (- n 1) 2))
             (right-size     (- n (+ left-size 1)))
                             
             (left-result    (partial-tree elts left-size))
             (non-left-elts  (cdr left-result))
             (right-elts     (cdr non-left-elts))
             (right-result   (partial-tree right-elts right-size))

             (this-entry     (car non-left-elts))
             (left-tree      (car left-result))
             (right-tree     (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry 
                         left-tree 
                         right-tree)
              remaining-elts))))
```

Читать эту лапшу было не супер приятно, поэтому я немного переформатировал.
Во первых я заменил ступеньки из ```let``` на ```let*```.

```let*``` это синтаксический сахар ступенек:
```
(let* ((a <expr 1>)      (let ((a <expr 1>))
       (b <expr 2>)  <=>   (let ((b <expr 2>))
       (c <expr 3>))         (let ((c <expr 3>))
  <body>)                      <body>)))    
```

По сути это очень похоже на серию присваиваний.
Во вторых очередность присваиваний я поменял, сгруппировав их больше по смыслу: сначала определяем размеры, потом считаем деревья, потом всё распаковываем аккуратно и идём дальше.

А что касается происходящего в алгоритме? 
Ну сначала мы первую половинку превращаем в дерево. Потом нам возвращают дерево и точку с которой продолжить процесс, мы отбрасываем верхнюю вершину дерева и строим правое поддерево. Всё вместе потом комбинируем.

Так как мы знаем что процедура возвращает сбалансированное поддеререво, то суммарная высота не больше ```log2 n/2 + 1 = log2 n```. Ну короче оно реально строит сбалансированное дерево.

Что касается асимптотики, то очевидно она равна O(n). 
В самом деле ```T(n) = 2 * T(n/2) + const```, по мастер-теореме такая белиберда это O(n).

## 2.65

```racket
#lang sicp

(define (entry tree) (cadr tree))
(define (left-branch tree) (car tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list left entry right))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size      (quotient (- n 1) 2))
             (right-size     (- n (+ left-size 1)))
                             
             (left-result    (partial-tree elts left-size))
             (non-left-elts  (cdr left-result))
             (right-elts     (cdr non-left-elts))
             (right-result   (partial-tree right-elts right-size))

             (this-entry     (car non-left-elts))
             (left-tree      (car left-result))
             (right-tree     (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry 
                         left-tree 
                         right-tree)
              remaining-elts))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (intersection-set set1 set2)
  (list->tree (intersection-list (tree->list set1)
                                 (tree->list set2))))

(define (union-set set1 set2)
  (list->tree (union-list (tree->list set1)
                          (tree->list set2))))

(define (intersection-list lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (let ((x1 (car lst1)) (x2 (car lst2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-list 
                         (cdr lst1)
                         (cdr lst2))))
              ((< x1 x2) (intersection-list 
                          (cdr lst1) 
                          lst2))
              ((< x2 x1) (intersection-list 
                          lst1 
                          (cdr lst2)))))))

(define (union-list lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      (append lst1 lst2)
      (let ((x1 (car lst1)) (x2 (car lst2)))
        (cond ((= x1 x2)
               (cons x1 (union-list 
                         (cdr lst1)
                         (cdr lst2))))
              ((< x1 x2)
               (cons x1 (union-list 
                          (cdr lst1) 
                          lst2))) 
              ((> x1 x2)
               (cons x2 (union-list 
                          lst1 
                          (cdr lst2))))))))
```

Ну у нас есть всё необходимое для этого: мы умеем пересекать и объединять за O(n) списки сортированные. Умеем деревья и списки друг в друга превращать за O(n), ну значит умеем деревья перескать за O(n).

## 2.66

```racket
#lang sicp

(define (entry tree) (cadr tree))
(define (left-branch tree) (car tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list left entry right))

(define (make-entry key value)
  (list key value))

(define key car)
(define value cadr)

(define (lookup given-key set)
  (cond ((null? set) false)
        ((= given-key (key (entry set)))
         (entry set))
        ((< given-key (key (entry set)))
         (lookup given-key (left-branch set)))
        (else
         (lookup given-key (right-branch set)))))
```

Пара примеров:
```
> (lookup 2 '(() (2 'hello) ()))
(2 'hello)
> (lookup 1 '((() (1 'world) ()) (2 'hello) ()))
(1 'world)
> (lookup 5 '((() (1 'world) ()) (2 'hello) (() (4 'nothig) (() (5 'something) ()))))
(5 'something)
> (lookup 5 '((() (1 'world) ()) (2 'hello) ()))
#f
```

# 2.3.4 Huffman Encoding Trees

## 2.67

```racket
#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
```

```
(A D A B B C A)
```

## 2.68

```racket
#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (collect branch)
    (cond ((leaf? branch) '())
          ((memq symbol (symbols (left-branch branch)))
           (cons 0 (collect (left-branch branch))))
          (else 
           (cons 1 (collect (right-branch branch))))))

  (if (memq symbol (symbols tree))
    (collect tree)
    (error "not a member:" symbol (symbols tree))))

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(#%require (only racket 
                 with-handlers 
                 exn-message
                 exn:fail?))

(define (expect-error thunk)
  (let ((message (with-handlers ([exn:fail? exn-message]) 
                   (thunk) 
                   false)))
    (if message
      (begin (display "[ok] : ") 
             (display message) 
             (newline))
      (error "error expected"))))

(if (equal? 
      (encode (decode sample-message sample-tree) sample-tree)
      sample-message)
  (begin (display "[ok] : messages are equal")
         (newline))
  (error "messages are not equal"))

(expect-error 
  (lambda ()
    (let ((decoded (decode sample-message sample-tree)))
      (encode (cons 'E decoded) sample-tree))))
```

Я немного вещей из racket поимпортировал, чтобы похендлить ошибки.
```
[ok] : messages are equal
[ok] : not a member: E (A B D C)
```

Что касается encode-symbol, всё достаточно просто, мы рекурсивно выбираем ветки в которые пойдём, а пойдём мы туда, где присутствует наш символ. Перед выходом из рекурсии надо не забыть приписать направление, куда мы пошли, если налево, то сделать ```(cons 0 <recur>)```, иначе ```(cons 1 <recur>)```.

## 2.69

> If you find yourself designing a complex procedure, then you are almost certainly doing something wrong. 

О да, детка, я делаю определенно не то, что меня попросили сделать.

Давайте по порядку: сделать то, что нас попросили достаточно просто. Что надо? Взять первый два элемента из множества, сделать из них дерево, засунуть обратно с помощью ```adjoin-set``` и повторить пока не останется единственный элемент. Просто? конечно просто. 

```racket
(define (successive-merge-simple trees)
  (cond ((null? trees) 
         (error "huffman tree cannot be empty"))
        ((null? (cdr trees))
         (car trees))
        (else 
         (successive-merge-simple 
           (adjoin-set (make-code-tree 
                         (car trees)
                         (cadr trees))
                       (cddr trees))))))

(define successive-merge successive-merge-simple)
```

Давайте прогоним через наши тесты:
```
[ok] : trees are equal
```

Тем не менее, на очень массивном алфавите с частотами в стиле: ```1 1 1 1 1 1 1 1 ... 1```, работать всё будет достаточно не шустро, потому что если сложить две единицы, то мы попрёмся аж в самый конец укладывать 2. Потом мы это будем делать и делать, пока не начнётся складывание двоек. И так по кругу при ```n = 2^k```. Короче ```O(n^2)```.

Ситуацию нормально так ускорит (при большом алфавите) замена ```set``` на ```tree-set```, например на AVL-дерево которое мы писали чуть ранее. И это будет порядка ```O(n log n)```. Так как мы сортируем всё равно значения в самом начале, то как бы и какая разница.

Но на самом деле, если значения у нас уже отсортированы, то мы можем всё сделать гораздо быстрее. А конкретно за линию. Ну как за линию, с учётом того, как дорого стоит конструкция деревьев, если мы мёржим множества, то за квадрат будет. Если не мержим ничего, то пошустрее. 

Я почему решил это включить? Ну потому что мне показалось интересным, как я буду реализовывать очереди в scheme, потому что алгоритм завязан на очередях. А я очереди еще не писал на функциональных языках.

Короче какое дело: вот у нас отсортированные деревья (по их весу лежат в списке допустим). Мы достаём два элемента, складываем и нам надо куда-то его деть. Возвращать обратно в список, как мы знаем неэффективно, поэтому давайте положим в какой-то еще список, пускай там лежит.
```
(1 2 3 4 5 6 7 8 9) |             (3 4 5 6 7 8 9) |   (3 4 5 6 7 8 9)
                    |> (+ 1 2)                    |> 
()                  |             ()              |   (3)
```

Теперь мы опять повторяем ту же операцию: надо взять два минимума и сложить их, результат где-то сохранить.
Ну так вот теперь у нас 2 случая: берем из списка 2 значения или берем из списка первое и из второго первое. Какая-то пара из них обязательно будет меньше вот их сложим и результат положим куда? В конец второго списка. Но почему? Потому что во втором списке теперь либо ничего не лежит, либо там лежит сумма предыдущих минимумов, а так как это предыдущие минимумы, уж точно они меньше текущей суммы.
```
(3 4 5 6 7 8 9)  |   (4 5 6 7 8 9)
                 |>           
(3)              |   (6)
```

Теперь вот выгодно сложить первые два в первом списке. И опять заметим, во втором у нас лежит предыдущая сумма минимумов, она точно меньше чем то, что мы сейчас сложим.
```
(4 5 6 7 8 9) |   (6 7 8 9)  |   (7 8 9) |   (9)       |   ()         |   ()      |   ()        
              |>             |>          |>            |>             |>          |>          
(6)           |   (6 9)      |   (9 12)  |   (9 12 15) |   (12 15 18) |   (18 27) |   (45)    
```

В общем идею, да и доказательство вы поняли: берем либо два из первой очереди, либо два из второй очереди, либо по одному из каждой. Пихаем результат во вторую очередь. На каждом шаге очереди остаются отсортированными, всё тип топ.

В общем осталось только научиться пихать в конец списка за ```O(1)```, что достигается с помощью всяких структур данных типа очередей.

Как можно реализовать эффективную очередь, если у тебя есть только связный список из структур данных? Ну на самом деле очень просто, метод называется два стэка. В нашем случае два списка. Первый назовём inbox, второй outbox. Из outbox мы только читаем, в inbox мы только пишем. 

Мы будем эксплуатировать тот факт, что перекидывая полностью один список в другой рекурсивно с помощью car, cons, мы его разворачиваем. И поэтому inbox магическим образом превращается в outbox.

То есть читаем из очереди пока в ней не опустеет outbox, потом перекидываем всё из inbox и радуемся, снова читаем. Если элемент попал в очередь то он единожды окажется в inbox и единожды окажется в outbox, thus, всего 2 раза будет прочитан суммарно за всё время жизни, поэтому амортизированно получается ```O(1)```.

Но у нас тут lisp, еще и в супер функциональном стиле, поэтому немного нестандартный подход будет: мы заместо того, чтобы в очереди перекидывать inbox в outbox во время операции pop, мы будем это делать сразу при конструировании очереди. Зачем? Чтобы реализовать ```O(1)``` операции ```first``` и ```drop```. Они у нас будут заменой мутабельного ```pop```, в нормальных языках программирования. 

Короче говоря следующий конструктор и следующие селекторы:

```racket
(define (make-queue inbox outbox)
  (if (null? outbox)
    (list '() (reverse inbox))
    (list inbox outbox)))

(define (inbox queue)
  (car queue))
(define (outbox queue)
  (cadr queue))
```

Достаточно легко делается теперь ```push```, ```first```, ```drop```.
```racket
(define (push v queue)
  (make-queue
    (cons v (inbox queue))
    (outbox queue)))

(define (empty-queue? queue)
  (and (null? (inbox queue))
       (null? (outbox queue))))

(define (first queue)
  (if (empty-queue? queue)
    false
    (car (outbox queue))))

(define (second queue)
  (first (drop queue)))

(define (drop queue)
  (if (empty-queue? queue) 
    queue
    (make-queue (inbox queue)
                (cdr (outbox queue)))))
```

Как вы можете заметить, если элемента нет, я возвращаю false, заместо того, чтобы дропнуть ошибку. Это нам еще пригодиться, потому что облегчит значительно жизнь.

Заведем еще несколько преобразований между списками и очередями и можно приступать к реализации:
```racket
(define (queue->list queue)
  (append (outbox queue)
          (reverse (inbox queue))))

(define (list->queue lst)
  (make-queue '() lst))
```

Теперь посмотрим на ту монструозную дичь и я немного поясню что происходит, после чего мы запустим тестик и порадуемся что всё работает:
```racket
(define (successive-merge-linear trees)
  ; add with infinity represented as false
  (define (+* a b)
    (cond ((not a) a)
          ((not b) b)
          (else (+ a b))))
  ; compare with infinity represented as false
  (define (<=* a b)
    (or (not b)
        (and a b
             (<= a b))))
  ; call weight or produce infinity
  (define (weight* tree)
    (if tree
      (weight tree)
      false))
  ; 
  ; to put it simply: 
  ;  1. take either first two from first queue
  ;  2. first two from second queue
  ;  3. or take first from both
  ; 
  ; follow the case weight of which is lower
  ; in the end push the value to the second queue
  ; 
  ; proceed until only one element in queue-2 is present
  ; 
  (define (iter queue-1 queue-2)
    (let ((from-first  (+* (weight* (first queue-1))
                           (weight* (second queue-1))))
          (from-second (+* (weight* (first queue-2))
                           (weight* (second queue-2))))
          (from-both   (+* (weight* (first queue-1))
                           (weight* (first queue-2)))))
      (cond ((and (empty-queue? queue-1)
                  (empty-queue? (drop queue-2)))
             (first queue-2))
            ((and (<=* from-first from-second)
                  (<=* from-first from-both))
             (iter (drop (drop queue-1))
                   (push (make-code-tree 
                           (first queue-1)
                           (second queue-1))
                         queue-2)))
            ((and (<=* from-second from-first)
                  (<=* from-second from-both))
             (iter queue-1
                   (push (make-code-tree 
                           (first queue-2)
                           (second queue-2))
                         (drop (drop queue-2)))))
            ((and (<=* from-both from-first)
                  (<=* from-both from-second))
             (iter (drop queue-1)
                   (push (make-code-tree 
                           (first queue-1)
                           (first queue-2))
                         (drop queue-2)))))))
  (cond ((null? trees)
         (error "huffman tree cannot be empty"))
        ((null? (cdr trees))
         (car trees))
        (else (iter (list->queue (cddr trees))
                    (list->queue (list (make-code-tree 
                                         (car trees)
                                         (cadr trees))))))))
```

Давайте будем считать, что false это +infinity, короче говоря поглощающее по сложению и нейтральное по минимуму. Почему false, ну потому что мы возвращаем эту ерунду, когда дёргаем first, second. Вот, теперь осталось написать варианты функций, которые пропогейтят нашу плюс бесконечность и сравнение соответственно. После чего надо сложить все три варианта и в каждом случае, в зависимости от того, какое из них минимально (вспоминаем одно из первых заданий) выбираем соответствующий вариант.

Давайте потестируем что-ли насколько хорошо получилось-то.

Проходим тест:
```
[ok] : trees are equal
```

А теперь более интересное, статистика по скорости работы двух вариантов, в зависимости от размера алфавита:
```
gen             simple          linear
1               0.29            0.21
2               0.44            1.31
3               0.79            2.61
4               1.02            4.15
5               1.35            5.32
6               1.76            6.54
7               2.36            8.14
8               2.62            12.47
9               2.93            9.62
10              3.39            11.48
20              14.47           32.16
30              21.45           33.61
40              36.12           43.96
50              53.57           55.83
60              75.7            65.27
70              92.96           74.13
80              123.35          86.48
90              162.36          95.69
100             183.1           109.63
200             735.01          227.24
300             1684.29         335.21
400             3058.31         442.97
500             4769.23         566.72
600             6910.14         678.24
700             9417.02         793.01
800             12190.14        897.9
900             15381.24        1024.19
1000            18967.19        1132.05
2000            75122.28        2436.22
3000            166620.15       3705.47
4000            293737.68       5101.39
5000            453729.96       6574.3
```

Ну, линейное прямо таки линейное... Но что хочется сказать: для большинства нормальных текстов, в которых нет тысяч символов, смысла эта линейность не несёт. Ну сколько русский + латиница, ну 200 символов. Там примерно паритет, а чуть раньше даже и обгоняет простой вариант.

А вот если китайский, да с эмодзи и юникодом... 
Вот это уже другое дело, там у них порядка 4000 знаков в обиходе гоняет.

Короче держите полный листинг и двигаемся дальше:
```racket
#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (collect branch)
    (cond ((leaf? branch) '())
          ((memq symbol (symbols (left-branch branch)))
           (cons 0 (collect (left-branch branch))))
          (else 
           (cons 1 (collect (right-branch branch))))))

  (if (memq symbol (symbols tree))
    (collect tree)
    (error "not a member:" symbol (symbols tree))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge-simple trees)
  (cond ((null? trees) 
         (error "huffman tree cannot be empty"))
        ((null? (cdr trees))
         (car trees))
        (else 
         (successive-merge-simple 
           (adjoin-set (make-code-tree 
                         (car trees)
                         (cadr trees))
                       (cddr trees))))))

(define (successive-merge-linear trees)
  ; add with infinity represented as false
  (define (+* a b)
    (cond ((not a) a)
          ((not b) b)
          (else (+ a b))))
  ; compare with infinity represented as false
  (define (<=* a b)
    (or (not b)
        (and a b
             (<= a b))))
  ; call weight or produce infinity
  (define (weight* tree)
    (if tree
      (weight tree)
      false))
  ; 
  ; to put it simply: 
  ;  1. take either first two from first queue
  ;  2. first two from second queue
  ;  3. or take first from both
  ; 
  ; follow the case weight of which is lower
  ; in the end push the value to the second queue
  ; 
  ; proceed until only one element in queue-2 is present
  ; 
  (define (iter queue-1 queue-2)
    (let ((from-first  (+* (weight* (first queue-1))
                           (weight* (second queue-1))))
          (from-second (+* (weight* (first queue-2))
                           (weight* (second queue-2))))
          (from-both   (+* (weight* (first queue-1))
                           (weight* (first queue-2)))))
      (cond ((and (empty-queue? queue-1)
                  (empty-queue? (drop queue-2)))
             (first queue-2))
            ((and (<=* from-first from-second)
                  (<=* from-first from-both))
             (iter (drop (drop queue-1))
                   (push (make-code-tree 
                           (first queue-1)
                           (second queue-1))
                         queue-2)))
            ((and (<=* from-second from-first)
                  (<=* from-second from-both))
             (iter queue-1
                   (push (make-code-tree 
                           (first queue-2)
                           (second queue-2))
                         (drop (drop queue-2)))))
            ((and (<=* from-both from-first)
                  (<=* from-both from-second))
             (iter (drop queue-1)
                   (push (make-code-tree 
                           (first queue-1)
                           (first queue-2))
                         (drop queue-2)))))))
  (cond ((null? trees)
         (error "huffman tree cannot be empty"))
        ((null? (cdr trees))
         (car trees))
        (else (iter (list->queue (cddr trees))
                    (list->queue (list (make-code-tree 
                                         (car trees)
                                         (cadr trees))))))))

(define successive-merge successive-merge-simple)
; (define successive-merge successive-merge-linear)

(define (make-queue inbox outbox)
  (if (null? outbox)
    (list '() (reverse inbox))
    (list inbox outbox)))

(define (inbox queue)
  (car queue))
(define (outbox queue)
  (cadr queue))

(define (push v queue)
  (make-queue
    (cons v (inbox queue))
    (outbox queue)))

(define (queue->list queue)
  (append (outbox queue)
          (reverse (inbox queue))))

(define (list->queue lst)
  (make-queue '() lst))

(define (empty-queue? queue)
  (and (null? (inbox queue))
       (null? (outbox queue))))

(define (first queue)
  (if (empty-queue? queue)
    false
    (car (outbox queue))))

(define (second queue)
  (first (drop queue)))

(define (drop queue)
  (if (empty-queue? queue) 
    queue
    (make-queue (inbox queue)
                (cdr (outbox queue)))))

;==== tests & miscelanious ====

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define generated 
  (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

(if (equal? generated sample-tree)
  (begin (display "[ok] : trees are equal")
         (newline))
  (error "trees are not equal"))

(define freq-max 999)

(#%require (only racket/mpair
                  mlist->list
                  list->mlist))
(#%require (only racket
                 sort))

(define (random-leafs n)
  (define (random-freqs-unsorted n)
    (if (zero? n)
      '()
      (cons (make-leaf n (inc (random freq-max)))
            (random-freqs-unsorted (dec n)))))
  (let ((freqs (random-freqs-unsorted n)))
     (list->mlist (sort (mlist->list freqs) #:key weight <))))

(define (random-test n tree-builder)
  (define leafs (random-leafs n))
  (define start (runtime))
  (tree-builder leafs)
  (- (runtime) start))

(define (random-tests n tree-builder)
  (define times 100)
  (define (time) (random-test n tree-builder))

  (do ((i 1 (inc i))
       (t 0 (+ t (time))))
    ((< times i) (exact->inexact (/ t times)))))

(define (make-table start stop step)
  (define (entry fn)
      (number->string
        (random-tests start fn)))
  (let ((simple (entry successive-merge-simple))  
        (linear (entry successive-merge-linear))
        (generation (number->string start)))
    (if (>= start stop)
      ""
      (string-append
        generation "\t\t" simple "\t\t" linear "\n"
        (make-table (+ start step) stop step)))))

(define (show-stats)
  (display (string-append 
             "gen\t\tsimple\t\tlinear\n"
             (make-table 1 10 1) 
             (make-table 10 100 10) 
             (make-table 100 1000 100)
             (make-table 1000 5001 1000))))

(show-stats)
```

## 2.70

```
type: huffman

encoded: 1111111001111011
         1000000000111111
         1001111011100000
         0000110101010101
         0101010101011101
                     1011

bits: 84

type: fixed-length

encoded: 1000001100110010
         0100100100100100
         1001100000110011
         0010010010010010
         0100100111110110
         1101101101101101
             101101011010

bits: 108
```

Ну вот как-то так оно. Особо комментировать я наверное не хочу происходящее.

```racket
#lang sicp

(#%require (only racket
                 string-upcase))

(#%require (only srfi/13
                 string-pad))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (collect branch)
    (cond ((leaf? branch) '())
          ((memq symbol (symbols (left-branch branch)))
           (cons 0 (collect (left-branch branch))))
          (else 
           (cons 1 (collect (right-branch branch))))))

  (if (memq symbol (symbols tree))
    (collect tree)
    (error "not a member:" symbol (symbols tree))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge trees)
  (cond ((null? trees) 
         (error "huffman tree cannot be empty"))
        ((null? (cdr trees))
         (car trees))
        (else 
         (successive-merge
           (adjoin-set (make-code-tree 
                         (car trees)
                         (cadr trees))
                       (cddr trees))))))

(define (zip a b)
  (if (or (null? a)
          (null? b))
    '()
    (cons (list (car a) (car b))
          (zip (cdr a) (cdr b)))))

(define (generate-bits n)
  (define (c x)
    (lambda (y) (cons x y)))
  (cond ((>= 0 n) '())
        ((= 1 n) '((0) (1)))
        (else 
         (let ((less (generate-bits (dec n))))
           (append (map (c 0) less)
                   (map (c 1) less))))))

(define (fixed-length-encoder alphabet)
  (let* ((n (length alphabet))
         (bits (ceiling (log n 2))))
    (zip (map car alphabet)
         (generate-bits bits))))

(define (fixed-encode-symbol symbol encoder)
  (cond ((null? encoder) 
         (error "unnable to find: " symbol 'in encoder))
        ((equal? symbol (caar encoder))
         (cadar encoder))
        (else 
         (fixed-encode-symbol symbol (cdr encoder)))))

(define (fixed-encode text encoder)
  (define (encode-sym x)
    (fixed-encode-symbol x encoder))
  (apply append (map encode-sym text)))

(define alphabet
  '((A    2)  (NA  16)
    (BOOM 1)  (SHA  3)
    (GET  2)  (YIP  9)
    (JOB  2)  (WAH  1)))


(define (symbol-upcase x)
  (string->symbol 
    (string-upcase 
      (symbol->string x))))

(define message
  (map symbol-upcase
       '(Get a job
         Sha na na na na na na na na

         Get a job
         Sha na na na na na na na na

         Wah yip yip yip yip 
         yip yip yip yip yip
         Sha boom)))

(define (wrap text len)
  (define total (string-length text))
  (define (next x) (min total (+ len x)))

  (do ((start 0   (next start))
       (end   len (next end))
       (collect '() (cons
                      (substring text start end) 
                      collect)))
    ((= start end) (reverse collect))))

(define (bitstring bits)
  (apply string-append
               (map number->string bits)))

(define huffman-tree (generate-huffman-tree alphabet))
(define encoded-message (encode message huffman-tree))

(define fixed (fixed-length-encoder alphabet))
(define fixed-encoded-message (fixed-encode message fixed))

(define (display-encoding encoded-message type)
  (display "type: ") (display type) 
  (newline) (newline)

  (define before "encoded: ")

  (define wrap-length 16)
  (define wrapped (wrap (bitstring encoded-message)
                        wrap-length))
  
  (define padding (+ wrap-length (string-length before)))
  (define (prepare line)
    (string-append (string-pad line padding) "\n"))


  (define with-before (cons (string-append before
                                           (car wrapped))
                            (cdr wrapped)))

  (display (apply string-append (map prepare with-before)))
  (newline)


  (display "bits: ") (display (length encoded-message))
  (newline) (newline))

(display-encoding encoded-message "huffman")
(display-encoding fixed-encoded-message "fixed-length")
```

## 2.71

Ну заметим, что каждый раз складывая пару, мы получаем на единицу меньше число, чем следующее.
В итоге алгос Хаффмана будет строить эдакий бамбук.

```
n = 5
            +
          / |
         +  16
       / |
      +  8
    / |
   +  4
 / |
1  2
```


``` 
n = 10

                        +
                      / |
                     +  512
                   / |
                  +  128
                / |
               +  64
             / |
            +  32
          / |
         +  16
       / |
      +  8
    / |
   +  4
 / |
1  2
```

В общем случае не сложно догадаться, что будет 1 бит на самый частый символ и ```n - 1``` бит на самый редкий.

## 2.72

Вспоминаем что у нас там был за ```encode-symbol```.

```racket
(define (encode-symbol symbol tree)
  (define (collect branch)
    (cond ((leaf? branch) '())
          ((memq symbol (symbols (left-branch branch)))
           (cons 0 (collect (left-branch branch))))
          (else 
           (cons 1 (collect (right-branch branch))))))

  (if (memq symbol (symbols tree))
    (collect tree)
    (error "not a member:" symbol (symbols tree))))
```

Ну что тут происходит? 
У нас дорогой memq, он стоит примерно столько сколько стоит алфавит.

А потом мы спускаемся на одну из веток ниже, и что-то опять там творим. В худшем случае наше дерево вырождается в бамбук (см предыдущее задание) и мы всегда ползём в левую ветку, убирая на 1 символ из сета. Это значит, что, чтобы закодировать символ нам надо сделать ```n - 1``` шаг, каждый раз вызывая ```memq```, который в худшем случае будет давать нагрузку ```n - 1```, ```n - 2```, ```n - 3``` ... 

Ну короче если прям вообще с бодуна оценивать то квадрат алфавита в худшем случае. 
memq можно заменить на что-нибудь более шустрое, ну например хранить множества как деревья, тогда мы будем стучать за log.

А с другой стороны если немного подумать и посчитать за сколько оно работает в среднем, или так скажем амортизированно? 

Ну давайте вероятность каждого символа ```a_i``` — ```p_i```.
Тогда можно оценить как-нибудь сверху в стиле: 

```
T(n) <= Sum[ n * len(a_i) * p_i ] <= n * E[huffman_l]
```

Ваще говоря можно получить более крутой результат, сейчас мы это сделаем. Для этого нам потребуется понятие энтропии, величины которую Клод Шенон придумал, чтобы разные задачи передачи сигналов и прочей белиберды решать и анализировать. Традиционно это как бы информационная емкость канала и определяется она как:

```
H[X] := E[I(x)] = E[-log p(X)]
```

Ну или иными словами
```
H[X] = - Sum [p(x) log(p(x))]
```

Где ```p(x)``` — вероятности символов.

Соответственно энтропия текста, это вот эта хитрая сумма вероятностей отдельных букв. Которые определяются как частота поделенная на длину текста. Или матожидание. Короче неважно.

Что более прикольно, так это то, что длины в дереве Хаффмана сверху оцениваются энтропией, а именно:
```
H[X] =< E[len] =< H[X] + 1
```

Это на самом деле не какой-то великий результат. Он на самом деле является конкатенацией двух других: 
1. Код Хаффмана оптимальный префиксный код (ну предоставьте другой и он ничем лучше не будет)
2. Существует префиксный код Шеннона длины кодов которого ```(ceil (log 1/p_x))```

Существование кода Шеннона следует из неравенства Крафта-Макмилана или что-то в этом роде.
Возьмите какую-нибудь лекцию на тему, ну например [эту](https://www.youtube.com/watch?v=j1_bAXAWHWM).

```
E[shannon_l] < H[x] + 1 (из свойств ```ceil```)
```

А так как код Шеннона это просто какой-то код, а код Хаффмана оптимальный, то
```
E[huffman_l] <= E[shannon_l]
```

Ну и отсюда получаем оценку.


Короче, к чему всё это, мы там выше оценивали время работы алгоса в среднем (пройдётся по всему тексту условно говоря), как O(n E[huffman_l]), ну так вот мы можем это оценить просто как O(n (1 + H(X))).

А из общих соображений, например неравенства Йенсена, мы можем получить оценку и на саму энтропию:

```
sum p_x log 1/p_x <= log (sum p_x 1/p_x) = log n
```

Итого всё вместе можно красивенько оценить как ```O(n log n)```.

В случае бамбука, который нам предлагают проанализировать, то там что-то вроде такой суммы получается:
```
sum [1 n] 2^k-n (n - k) = sum [1 n] k 2^-k <= sum [1, +inf] k/2^k = 2
```

Поэтому код Хаффмана на бамбуке работать будет за ```O(n)```. 
В общем случае за ```n log(n)```.

Если улучшить memq с помощью деревьев, то вообще красота получится: ```O(log^2 n)```.
