# 2.5.1 Generic Arithmetic Operations

## 2.77

Итак у нас не работает селектор для комплексных чисел. 

```
(magnitude (make-complex-from-mag-ang 1 2))
; get: method magnitude for type (complex) does
 not exist [,bt for context]
```

Книга предлагает добавить следующий код, чтобы всё исправить:

```racket
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)
```

Ну и действительно:
```
> (magnitude (make-complex-from-mag-ang 1 2))
1
```

Почему это работает?

Ну смотрите, что делает ```magnitude```? ```magnitude``` достаёт тип комплексного числа, по нему ищет соответствующую операцию, выбрасывает тип и делегирует найденной операции задачу. 

Когда мы вызываем ```magnitude``` на типе ```complex```, то происходит делегация ни к чему иному как к ```magnitude``` в очередной раз. А так как в этот раз это уже комплексное число определенного типа (```polar``` или ```rectangle```), то всё получается.

На конец нам предлагают потрейсить вызовы ```apply-generic```. Для этого мы воспользуемся стандартным ```racket/trace``` 

```
> (trace apply-generic)
> (magnitude (make-complex-from-mag-ang 1 2))
>{apply-generic magnitude (complex polar 1 . 2)}
>{apply-generic magnitude (polar 1 . 2)}
<1
1
```

И мы видим, что вызывается оно ровно два раза, как мы это и предсказали выше.
В первом случае вызывается вновь ```magnitude```, а во втором уже внутренняя реализация ```magnitude-polar``` или что-то на вроде этого.

## 2.78

Нас просят так видоизменить нашу программу, чтобы сложение чисел было прозрачным, заместо ```(add (make-scheme-number 1) (make-scheme-number 1))```, чтобы было ```(add 1 2)```.

Для этого нам надо видоизменить поведение ```attach-type-tag```, ```type-tag``` и ```contents``` соответственно.

Для этого мы теперь запретим не-числам ставить тайптег ```scheme-number```, а сами числа видоизменять не будем.
```type-tag``` и ```contents``` теперь тоже пусть просто проверят, что нам поступило число и выкинут его или выкинут его тип: ```scheme-number```, в зависимости от того, что мы вызвали.

```racket
(define (attach-tag type-tag contents)
  (cond ((and (eq? 'scheme-number)
              (number? contents))
         contents)
        ((and (eq? 'scheme-number)
              (not (number? contents)))
         (error 'attach-tag
                "trying to attach tag scheme-number to non scheme number"))
        (else 
         (cons type-tag contents))))
        
(define (type-tag datum)
  (cond ((pair? datum)
         (car datum))
        ((number? datum)
         'scheme-number)
        (else 
         (error 'type-tag 
                "bad tagged datum ~a" datum))))
    

(define (contents datum)
  (cond ((pair? datum) 
         (cdr datum))
        ((number? datum)
         datum)
        (else 
         (error 'contents 
                "bad tagged datum ~a" datum))))
```

And it works!
```
> (mul 3 3)
9
```

## 2.79

Итак нас просят добавить generic оператор сравнения.

```racket
(define (equ? x y) (apply-generic 'equ? x y))
```

- пакет **scheme-number**
  ```racket 
  (put 'equ? '(scheme-number scheme-number) =)
  ```
- пакет **rational**

  Я немного поменял конструктор на тот что мы делали в домашнем задании:
  ```racket
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
  ```
  
  И теперь легко определить равенство:
  ```racket
  (define (eq-rat? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))

  (put 'equ? '(rational rational) eq-rat?)
  ```
- пакет **complex**
  ```racket
  (define (eq-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))

  (put 'equ? '(complex complex) eq-complex?)
  ```

  Вообще надо заметить, операторы сравнения это не самая тривиальная штука и всегда к ним надо подходит, что говорится, with great care.

И мы можем радоваться:

```
> (equ? 1 1)
#t
> (equ? 1 2)
#f
> (equ? (make-rational 1 2) (make-rational -1 -2))
#t
> (equ? (make-rational 1 2) (make-rational -1 2))
#f
> (equ? (add (make-complex-from-mag-ang 1 2) (make-complex-from-real-imag 3 4))
        (add (make-complex-from-mag-ang 1 2) (make-complex-from-real-imag 3 4)))
#t
> (equ? (add (make-complex-from-mag-ang 4/3 6)
             (add (make-complex-from-real-imag 3 4/3) 
                  (make-complex-from-mag-ang 1 2)))
        (add (add (make-complex-from-mag-ang 4/3 6) 
                  (make-complex-from-real-imag 3 4/3))
             (make-complex-from-mag-ang 1 2)))
#f
```

В последнем примере демонстрируется что сложение числе с плавающей точкой не ассоциативно :).

## 2.80

На ум на самом деле приходит что-то в стиле:
```racket
(define (=zero? x)
  (equ? (sub x x) x))
```

Единственное что, можем ли мы вообще такого требовать?
Ну наверное да... А может быть нет :). 

Потому что у нас язык MIT Scheme или скорее даже r5rs, а в стандарте написано:
> An implementation may use floating point and other approximate representation strategies for inexact numbers. This report recommends, but does not require, that the IEEE 32-bit and 64-bit floating point standards be followed by implementations that use flonum representations, and that implementations using other representations should match or exceed the precision achievable using these floating point standards. 

Там дальше немного про то как они себя вести должны, но в целом строгих гарантий у нас... Наверное нет.

Поэтому давайте сделаем по-тупому, как в прошлом задании:

```racket
(define (=zero? x)
  (apply-generic '=zero? x))
```

- пакет **scheme-number**
  ```racket 
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  ```
- пакет **rational**
  ```racket
  (define (=zero? x)
    (= (numer x) 0))

  (put '=zero? '(rational) =zero?)
  ```
- пакет **complex**
  ```racket
  (define (=zero? z1)
    (= (magnitude z1) 0))

  (put '=zero? '(complex) =zero?)
  ```

Ну и пара тестиков:
```
> (=zero? 1)
#f
> (=zero? 0)
#t
> (=zero? (sub (make-rational 1 2) (make-rational 1 2)))
#t
> (=zero? (make-rational 1 2))
#f
> (=zero? (make-complex-from-mag-ang 0 0.5))
#t
> (=zero? (make-complex-from-mag-ang 1 0))
#f
> (=zero? (make-complex-from-real-imag 0 0))
#t
> (=zero? (make-complex-from-real-imag 1 1))
#f
```

# 2.5.2 Combining Data of Different Types

Сперва добавим немножко новых методов: ```get-coercion```, ```put-coercion```.

Для этого немножко реструктуризуем то что у нас раньше было.
А я напомню было что-то такое:
```racket
;; generic table 

(#%require (only racket
                 make-hash
                 hash-set!
                 hash-ref))

(define *generics-table*
  (make-hash))

(define (put method type value)
  (let ((key (cons method type)))
    (if (not (hash-ref *generics-table* key false))
      (hash-set! *generics-table* key value)
      (error 'put
             "method ~a for type ~a already defined"
             method type))))

(define (get method type)
  (let ((result (hash-ref 
                  *generics-table* 
                  (cons method type) 
                  false)))
    (if (not result)
      (error 'get 
             "method ~a for type ~a does not exist"
             method type))
    result))

(define (has? method types)
  (if (hash-ref *generics-table* 
                (cons method types) 
                false)
    true false))
```

Давайте сделаем какой-то более универсальный способ создавать подобные таблицы: откажемся от глобальной переменной. И сделаем какой-то универсальный интерфейс.

```racket
(#%require (only racket
                 make-hash
                 hash-set!
                 hash-ref))

(define (setup-table . name-list)
  (define name
    (cond 
      ((= 0 (length name-list))
       'unonymous)
      ((= 1 (length name-list))
       (car name-list))
      (else 
       (error 'setup-table
              "expected 0 or 1 argument"))))

  (define table
    (make-hash))

  (define (put key-1 key-2 procedure)
    (let ((key (cons key-1 key-2)))
      (if (not (hash-ref table key false))
        (hash-set! table key procedure)
        (error 'put
               "procedure for keys ~a ~a already defined"
               key-1 key-2))))

  (define (get key-1 key-2)
    (hash-ref 
      table 
      (cons key-1 key-2) 
      false))

  (define (has? key-1 key-2)
    (if (get key-1 key-2)
      true
      false))

  (lambda (key . args)
    (apply 
      (cond ((eq? key 'put) 
             put)
            ((eq? key 'get)
             get)
            ((eq? key 'has?)
             has?)
            (else
              (error 'table
                     "unknown method for ~a table"
                     name)))
      args)))

(define generics (setup-table 'generics))
(define coercion (setup-table 'coercion))
```

Мы можем теперь добавить соответствующий код, чтобы наши примеры работали из коробки.
```racket
(define (put key-1 key-2 method)
  (generics 'put key-1 key-2 method))
(define (get key-1 key-2)
  (generics 'get key-1 key-2))

(define (put-coercion key-1 key-2 method)
  (coercion 'put key-1 key-2 method))
(define (get-coercion key-1 key-2)
  (coercion 'get key-1 key-2))
```

Но я лично предпочитаю немножко переписать код из упражнений, потому что мне нравится самодокументируемость того, что я вызываю ```put``` и ```get``` из таблицы названной ```generics```.

```racket
(define (apply-generic op . args)
  (define type-tags (map type-tag args))

  (define (method-not-found)
    (error 'apply-generic
           "method not found ~a ~a"
           op type-tags))

  (define (coerce-2 op a1 a2)
    (let* ((type1 (type-tag a1))
           (type2 (type-tag a2))
           (t1->t2 (coercion 'get type1 type2)) 
           (t2->t1 (coercion 'get type2 type1)))
      (cond (t1->t2
             (apply-generic op (t1->t2 a1) a2))
            (t2->t1
             (apply-generic op a1 (t2->t1 a2)))
            (else
             (method-not-found)))))
      
  (let ((proc (generics 'get op type-tags)))
      (cond (proc
             (apply proc (map contents args)))
            ((= (length args) 2)
             (apply coerce-2 op args))
            (else
             (method-not-found)))))
```

## 2.81

Итак, мы действительно при отсутствии метода с типами T, T, сделаем fallback в coercion и попробуем привести тип T к типу T. А потом рекурсивно запуститься, как бы реализуя стратегию перезагрузки.

И в ответ на **первый** вопрос, что произойдёт, если мы предоставим следующие методы приведения:

```racket
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(coercion 'put 'scheme-number 'scheme-number
          scheme-number->scheme-number)

(coercion 'put 'complex 'complex 
          complex->complex)
```

И вызовем apply-generic на чём-нибудь, что не имеет процедуры для соответствующих типов, например:
```racket
(define (exp x y) 
  (apply-generic 'exp x y))
```

В пакете **scheme-number**:
```
(generics 'put 'exp '(scheme-number scheme-number) 
            (lambda (x y) (tag (expt x y))))
```

Поведение весьма очевидно: мы упадём в вечный цикл. 
Давайте даже проверим:
```
> (#%require racket/trace)
> (trace apply-generic)
> (exp (make-complex-from-real-imag 1 2)
       (make-complex-from-real-imag 3 4))

>{apply-generic exp (complex rectangular 1 . 2) (complex rectangular 3 . 4)}
>{apply-generic exp (complex rectangular 1 . 2) (complex rectangular 3 . 4)}
>{apply-generic exp (complex rectangular 1 . 2) (complex rectangular 3 . 4)}
>{apply-generic exp (complex rectangular 1 . 2) (complex rectangular 3 . 4)}
>{apply-generic exp (complex rectangular 1 . 2) (complex rectangular 3 . 4)}
>{apply-generic exp (complex rectangular 1 . 2) (complex rectangular 3 . 4)}
>{apply-generic exp (complex rectangular 1 . 2) (complex rectangular 3 . 4)}
>{apply-generic exp (complex rectangular 1 . 2) (complex rectangular 3 . 4)}
>{apply-generic exp (complex rectangular 1 . 2) (complex rectangular 3 . 4)}
...
```

Это конечно не адекватное поведение, потому что конечно самоприведение — деяние странное, потому что если бы мы нашли операцию с такими типами, то мы бы всё равно её нашли, в этом смысле самоприведение, сохраняя тип не решает никакой задачи. Но теоретически конечно такая вещь в графе, таблице или в отношении приведений штука допустимая. И программа наша не должна выпадать в осадок от того, что нам такое дали.

На самом деле пассаж выше это ответ на **второй** вопрос, потому что мы, давайте я сокращу, обозначили проблему: у нас случается некорректное поведение программы, если какой-то разработчик добавит приведение типа к самому себе.

И решением **третьего** пункта будет следующий код:
```racket
(define (apply-generic op . args)
  (define type-tags (map type-tag args))

  (define (method-not-found)
    (error 'apply-generic
           "method not found ~a ~a"
           op type-tags))

  (define (coerce-2 op a1 a2)
    (let* ((type1 (type-tag a1))
           (type2 (type-tag a2))
           (t1->t2 (coercion 'get type1 type2)) 
           (t2->t1 (coercion 'get type2 type1)))
      (cond (t1->t2
             (apply-generic op (t1->t2 a1) a2))
            (t2->t1
             (apply-generic op a1 (t2->t1 a2)))
            (else
             (method-not-found)))))
      
  (let ((proc (generics 'get op type-tags)))
      (cond (proc
             (apply proc (map contents args)))
            ((and (= (length args) 2)
                  (not (apply equal? type-tags)))
             (apply coerce-2 op args))
            (else
             (method-not-found)))))
```

Давайте запустим наш предыдущий пример и увидим, что он корректно работает:
```
> (exp (make-complex-from-real-imag 1 2)
       (make-complex-from-real-imag 3 4))
; apply-generic: method not found exp (complex complex) 
; [,bt for context]
```

## 2.82

Нас попросили обобщить приведения для нескольких типов. Ну и давайте это сделаем.
На самом деле есть много разных стратегий, но одна из них, да, попытаться привести все аргументы к единому типу, который есть среди аргументов.

Для этого мы пройдёмся по всем типам, покастим все аргументы к этому типу. Если получилось, то попробуем найти метод и если он существует — применим его.

Что-то вроде этого:
```racket
(define (apply-generic op . args)
  (define type-tags (map type-tag args))

  (define (method-not-found)
    (error 'apply-generic
           "method not found ~a ~a"
           op type-tags))

  (define (search args)
    (generics 'get op (map type-tag args)))

  (define (has? args)
    (if (search args)
      true false))

  (define (reflective-coercion type-1 type-2)
    (if (equal? type-1 type-2)
      identity
      (coercion 'get type-1 type-2)))

  (define (cast-all to args)
    (if (null? args)
      '()
      (let* ((arg (car args))
             (rest (cdr args))
             (from (type-tag arg))
             (from->to (reflective-coercion from to))
             (rest-casted (cast-all to rest)))
        (if (and from->to
                 rest-casted)
          (cons (from->to arg)
                rest-casted)
          false))))

  (define (first p? seq)
    (if (null? seq)
      false
      (if (p? (car seq))
        (car seq) 
        (first p? (cdr seq)))))

  (define (filter-map f seq)
    (apply append
      (map (lambda (x)
             (let ((r (f x)))
               (if r (list r) '())))
           seq)))

  (define (coerce-search args)
    (define (cast-args type)
      (cast-all type args))
    (first has? 
           (filter-map cast-args type-tags)))

  (define (equal-all? . rest)
    (cond ((null? rest) true)
          ((null? (cdr rest)) true)
          (else
           (let ((a (car rest))
                 (b (cadr rest))
                 (rest (cddr rest)))
             (and (equal? a b)
                  (apply equal-all? rest))))))
      

  (define (apply-strip args)
    (apply (search args) (map contents args)))

  (cond ((has? args)
         (apply-strip args))
        ((apply equal-all? args)
         (method-not-found))
        (else
         (let ((c-args (coerce-search args)))
           (if c-args
             (apply-strip c-args)
             (method-not-found))))))
```

Давайте проверим, что у нас ничего не сломалось:
```
> (add 1 1)
2
> (add 1 (make-complex-from-real-imag 1 2))
(complex rectangular 2 . 2)
```

И давайте введем какой-то метод из, не знаю, трёх комплексных чисел:
```racket
(define (mul-3 a b c)
   (apply-generic 'mul-3 a b c))
```

В пакете **complex**:
```racket
(define (mul-3 a b c)
  (make-from-mag-ang
    (* (magnitude a) (magnitude b) (magnitude c))
    (+ (angle a) (angle b) (angle c))))

(generics 'put 'mul-3 
          '(complex complex complex)
          (lambda (a b c)
            (tag (mul-3 a b c))))
```

И посмотрим что получится:
```
> (mul-3 1 2 (make-complex-from-mag-ang 1 2)) 
(complex polar 4.47213595499958 . 1.1071487177940904)
> (mul-3 1 2 3)
; apply-generic: method not found mul-3 (scheme-number scheme-number scheme-number) 
; [,bt for context]
```

Отвечая на последний вопрос: пример, когда подобная техника не достаточно общая — как раз то, что мы видим выше, а именно случай, когда у нас числа могут быть приведены к комплексным, но они не приведены. И второй случай это какие-то смешанные операции вроде с типами вроде ```(rational rational integer)```, например это может быть операция нахождения корней числа в конечном поле, и мы если закинем что-нибудь вида (integer rational integer), то проиграем. Потому что у нас будут расмотрены варианты: (integer integer integer) и (rational rational rational), но не (rational rational integer).

## 2.83

Итак, давайте сначала разработает механизм с помощью которого мы будем работать с башней типов и только потом реализуем проверим на каком-нибудь примере.

Я считаю что вопросы дел башни — вопросы энумерации типов, нахождения предков и т.д. и т.п.
Поэтому приведения мы всё так же будем хранить в таблице coercion. 

И raise будет комбинацией взаимодействия с tower и coercion.

```racket
(define (setup-type-tower)
  (define tower (setup-table 'tower))

  (define (prev type)
    (tower 'get 'prev type))

  (define (next type)
    (tower 'get 'next type))

  (define (first)
    (tower 'get 'first false))

  (define (last)
    (define (scroll type)
      (if (next type)
        (scroll (next type))
        type))
    (scroll (first)))

  (define (registred)
    (define (scroll type result)
      (if type
        (scroll (next type) (cons type result))
        (reverse result)))
    (scroll (first) '()))

  (define (register new-type)
    (if (in new-type)
      (error 'register 
             "type is already registred"))
    (tower 'put 'in new-type true)
    (for-each (lambda (old-type)
                (tower 'put '< 
                       (list old-type new-type)
                       true))
              (registred))

    (if (not (first))
      (tower 'put 'first false new-type)
      (let ((last-type (last)))
        (tower 'put 'next last-type new-type)
        (tower 'put 'prev new-type last-type))))

  (define (restrict fn)
    (define (restrict-param type)
      (if (not (in type))
        (error 'tower
               "type ~a is not yet registred"
               type)))
    (lambda params      
      (for-each restrict-param
                params)
      (apply fn params)))

  (define (=t lhs rhs)
    (restrict lhs) (restrict rhs)
    (equal? lhs rhs))

  (define (<t lhs rhs)
    (restrict lhs) (restrict rhs)
    (tower 'get '< (list lhs rhs)))

  (define (>t lhs rhs)
    (<t rhs lhs))

  (define (<=t lhs rhs)
    (or (=t lhs rhs)
        (<t lhs rhs)))

  (define (>=t lhs rhs)
      (or (=t lhs rhs)
          (>t lhs rhs)))
  
  (define (in type)
    (tower 'get 'in type))

  (let ((export-methods
         (list (list 'register register)
          (list 'registred registred)
          (list 'in in)
          (list 'next (restrict next))
          (list 'prev (restrict prev))
          (list '= (restrict =t))
          (list '< (restrict <t))
          (list '<= (restrict <=t))
          (list '> (restrict >t))
          (list '>= (restrict >=t)))))
    (lambda (action . params)
      (let ((result (assq action export-methods)))
        (if result 
          (apply (cadr result) params)
          (error 'tower 
                 "unknown method ~a"
                 action))))))
```

Немножечко я бы сказал, что verbose, но работу свою делает.
Мы просто делаем:
```racket
(tower 'register 'integer)
(tower 'register 'rational)
(tower 'register 'real)
(tower 'register 'complex)
```

И можем посмотреть на результат:
```
> (tower 'registred)
(integer rational real complex)
> (tower 'next 'integer)
rational
> (tower 'prev 'integer)
#f
> (tower 'next 'wonderful)
; tower: type wonderful is not yet registred [,bt for context]
> (tower '< 'integer 'complex)
#t
> (tower '> 'integer 'complex)
#f
> (tower '<= 'integer 'complex)
#t
> (tower '>= 'integer 'complex)
#f
> (tower '>= 'integer 'integer)
#t
> (tower '< 'integer (tower 'next 'integer))
#t
> (tower '< 'wonderful 'complex)
; tower: type wonderful is not yet registred [,bt for context]
```

Теперь перейдём к реализации raise.
Всё здесь достаточно просто получаем следующий тип, если его нет, выбрасываем ошибку.
Если есть то ищем в таблице coercion процедуру, если таковой не найдётся, выбрасываем ошибку.
Если всё есть, то применяем к аргументу процедуру.

```racket
(define (raise obj)
  (let* ((type (type-tag obj))
         (next (tower 'next type))
         (type->next (coercion 'get type next)))
    (cond ((not next)
           (error 'raise
                  "cannot raise further ~a"
                  type))
          ((not type->next)
           (error 'raise
                  "cannot find coercion ~a->~a"
                  type next)))
    (type->next obj)))
```

Теперь осталось протестировать это поделие. 
Для этого нам надо предоставить соответствующие типы данных.

И у нас небольшая проблема: scheme по умолчанию идёт уже с этими типами данных. Причём между ними происходит всякая нехорошая конверсия в духе того, что мы хотим установить. Поэтому можно получить какие-то рандомные неожиданные баги.

Чтобы это немного устранить, давайте сначала грохнем пакет scheme-number и избавимся от implicit конверсий, которые мы ввели раньше. 

```racket
(define (attach-tag type-tag contents)
  (cons type-tag contents))
        
(define (type-tag datum)
  (cond ((pair? datum)
         (car datum))
        (else 
         (error 'type-tag 
                "bad tagged datum ~a" datum))))
    
(define (contents datum)
  (cond ((pair? datum) 
         (cdr datum))
        (else 
         (error 'contents 
                "bad tagged datum ~a" datum))))

(define (apply-generic op . args)
  (define (method-not-found)
    (error 'apply-generic
           "method not found ~a ~a"
           op (map type-tag args)))
  (define (search args)
    (generics 'get op (map type-tag args)))
  (define (has? args) (if (search args) true false))
  (define (apply-strip args)
    (apply (search args) (map contents args)))
  (if (has? args)
    (apply-strip args)
    (method-not-found)))
```

И сделаем пакет integer:

```racket
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))

  (define (div a b)
    (let ((make (generics 'get 'make 'rational)))
      (if make
        (make a b)
        (error 'integer-package/div
               "rational package is not installed"))))

  (define (make x) 
    (if (not (integer? x))
      (error 'integer-package/make
             "expected integer but got ~a"
             x))
    (tag x))

  (generics 'put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (generics 'put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (generics 'put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (generics 'put 'div '(integer integer) div)
  (generics 'put 'equ? '(integer integer) =)
  (generics 'put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (generics 'put 'make 'integer make)
  'done)
```

Заметьте кстати что я не умею делить целые числа, поэтому я просто делегирую эту внутри пакета другому пакету.
И в случае если я это сделать не смогу, то я упаду в ошибку.

Другой подход мог бы заключаться в том, чтобы вообще не предоставлять подобный метод (ну а зачем в самом деле). И заместо этого позволить ```apply-generic``` найти соответствующий метод у ```rational```, привести ```integer->rational``` и произвести деление уже там. Давайте это сейчас запомним и в следующий раз протестируем.

```racket
(define (make-integer n)
  ((generics 'get 'make 'integer) n))
(install-integer-package)
```

```
> (add (make-integer 1) (make-integer 2))
(integer . 3)
> (make-integer 1)
(integer . 1)
> (make-integer 1.5)
; integer-package/make: expected integer but got 1.5 [,bt for context]
> (div (make-integer 1) (make-integer 2))
; integer-package/div: rational package is not installed [,bt for context]
```

```rational``` это всё тот же пакет, за исключением может быть конструктора, но я не буду как-то ограничивать на самом деле входные данные, пофигу

```real``` это по-сути переименованный ```scheme-number```, за исключением ```make```, который тоже теперь должен выбрасывать ошибку:
```racket
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (define (make x)
    (if (not (real? x))
      (error 'real-package/make
             "expected real but got ~a"
             x))
      (tag x))
  (generics 'put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (generics 'put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (generics 'put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (generics 'put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (generics 'put 'equ? '(real real) =)
  (generics 'put '=zero? '(real)
       (lambda (x) (= x 0)))
  (generics 'put 'make 'real make)
  'done)
```

И complex это всё тот же пакет, плюс я добавлю: 
```racket
(define make-complex make-complex-from-real-imag)
```

Теперь давайте проверим, что у нас ничего не работает: мы подготовили для этого почву!

```
> (raise (make-integer 1))
; raise: cannot find coercion integer->rational [,bt for context]
```

Отлично, у нас падает всё по той причине, что не нашлось соответствующего приведения, давайте добавим!
```racket
(define (install-numerical-tower-package)
  (tower 'register 'integer)
  (tower 'register 'rational)
  (tower 'register 'real)
  (tower 'register 'complex)

  (define (make-rational a b)
    ((generics 'get 'make 'rational) a b))
  (define (make-complex a b)
    ((generics 'get 'make-from-real-imag 'complex) a b))
  (define (make-real x)
    ((generics 'get 'make 'real) x))

  (define (integer->rational i)
    (make-rational (contents i)
                   1))
  (define (rational->real r)
    (make-real (/ (numer r) (denom r))))
  (define (real->complex r)
    (make-complex (contents r) 0))

  (coercion 'put 'integer 'rational integer->rational)
  (coercion 'put 'rational 'real rational->real)
  (coercion 'put 'real 'complex real->complex)
  'done)
```

Я их не добавляю в пакеты, потому что по-существу это не работа для индивидуальных пакетов.
А де-факто работа отдельного пакета.


Давайте наконец посмотрим на наше чудо-юдо:
```
> (raise (make-integer 2))
(rational 2 . 1)
> (raise (raise (make-integer 2)))
(real . 2)
> (raise (raise (raise (make-integer 2))))
(complex rectangular 2 . 0)
> (raise (raise (raise (raise (make-integer 2))))) 
; raise: cannot raise further complex [,bt for context]
> (raise (attach-tag 'wonderful '()))
; tower: type wonderful is not yet registred [,bt for context]
```

## 2.84

Ну давайте сделаем наш apply generic: если не нашли метод, то попробуем повысить тип до некого единого.
И если это не получится то попробуем еще раз поднять тип, пока не упрёмся в максимальный.

```racket
(define (apply-generic op . args)
  (define (tags args)
    (map type-tag args))

  (define (method-not-found)
    (error 'apply-generic
           "method not found ~a ~a"
           op (tags args)))

  (define (search args)
    (generics 'get op (tags args)))
  (define (has? args) 
    (generics 'has? op (tags args)))
  
  (define (apply-strip args)
    (apply (search args) (map contents args)))

  (define (filter p? seq)
    (if (null? seq)
      seq
      (if (p? (car seq))
        (cons (car seq)
              (filter p? (cdr seq)))
        (filter p? (cdr seq)))))

  (define (tower-tags args) 
    (filter 
      (lambda (type)
        (tower 'in type))
      (tags args)))

  (define (can-be-raised? args)
    (define (can-raise? type)
      (if (tower 'next type)
        true false))
    (define (loop types)
      (and (not (null? types))
           (or (can-raise? (car types))
               (loop (cdr types)))))
    (loop (tower-tags args)))

  (define (max-type args)
    (define (loop types)
      (let ((first (car types))
            (rest (cdr types)))
        (if (null? rest)
          first
          (let ((second (loop rest)))
            (if (tower '< first second)
              second
              first)))))
    (loop (tower-tags args))) 

  (define (raise-to type arg)
    (let ((arg-type (type-tag arg)))
      (if (or (not (tower 'in arg-type))
              (equal? type arg-type))
        arg
        (raise-to type (raise arg)))))

  (define (raise-to-common args)
    (map  
      (lambda (arg)
        (raise-to (max-type args) arg))
      args))

  (define (raise-all args)
    (let ((result (raise-to-common args)))
      (if (not (equal? (tags result) 
                       (tags args)))
        result
        (map (lambda (arg)
               (if (tower 'in (type-tag arg))
                 (raise arg)
                 arg))
             result))))

  (cond ((has? args)
         (apply-strip args))
        ((can-be-raised? args)
         (apply apply-generic op (raise-all args)))
        (else 
         (method-not-found))))
```

Ну в общем-то давайте я на всякий случай прокомментирую:
- tower-tags достаёт из аргументов типы, которые находятся в таблице
- max-type ищет максимальный тип
- raise-to делает raise до соответствующего типа
- raise-to-common делает raise до общего типа среди существующих
- raise-all делает raise-to-common или повышает на один уровень выше

Таким образом когда мы делаем например ```(div (make-integer 1) (make-real 2))```, мы получаем ```(real . 1/2)```.
А если мы например сделаем ```(magnitude (make-integer 1))```, мы будем поднимать тип до того момента, пока он не станет комплексным числом и получим соответственно 1.

Напомню, что в пакете integer определение div было следующим:
```racket
(define (div a b)
  (let ((make (generics 'get 'make 'rational)))
    (if make
      (make a b)
      (error 'integer-package/div
             "rational package is not installed"))))
```

Давайте его удалим, потому что у нас теперь есть более совершенный механизм, который позволяет деление делегировать рациональным числам.


И потестим:

```racket
> (div (make-integer 1) (make-integer 2))
(rational 1 . 2)
> (mul (make-integer 2) (make-complex-from-mag-ang 3 4))
(complex polar 6 . 4)
> (add (make-complex 1 3) (make-rational 1 2))
(complex rectangular 3/2 . 3)
> (real-part (make-integer 1))
1
> (angle (make-integer 1))
0
> (imag-part (make-integer 2))
0
> (numer (make-integer 3))
3
> (denom (make-integer 3))
1
> (numer (make-real 1))
; apply-generic: method not found numer (complex) [,bt for context]
```

Ну... Последнее сообщение показывает некоторую проблему в нашей реализации.
Она на самом деле легко фиксится, потому что нам надо просто проэмулировать рекурсивное применение.

Чем-нибудь вроде такого:
```racket
(define (apply-generic op . args)
  ; ...
  (define (apply-loop args)
    (cond ((has? args)
           (apply-strip args))
          ((can-be-raised? args)
           (apply-loop (raise-all args)))
          (else 
            (method-not-found))))
  (apply-loop args))
```

И теперь всё работает как положено: 
```
> (numer (make-real 1))
; apply-generic: method not found numer (real) [,bt for context]
```

Давайте попробуем еще маленькую деталь, а именно случай, когда raise мы делаем с участием аргументов, которые в башне не находятся.

```racket
(define (wonderful w i)
  (apply-generic 'wonderful w i))

(define (make-wonderful message)
  (attach-tag 'wonderful message))

(define (display-n message n)
  (if (not (zero? n))
    (begin 
      (display message)
      (display-n message (dec n)))))

(generics 'put 'wonderful '(wonderful rational)
     (lambda (message rat)
       (let ((rat (attach-tag 'rational rat)))
         (display-n message (numer rat)) (newline)
         (display "-") (newline)
         (display-n message (denom rat)) (newline))))
```

```
> (wonderful (make-wonderful "wow!") (make-integer 3))
wow!wow!wow!
-
wow!
```

Ну в общем прекрасно работает. А на этом в этом задании всё.

## 2.85

Ну это на самом деле почти что копипаста raise
```racket
(define (project obj)
  (let* ((type (type-tag obj))
           (prev (tower 'prev type))
           (type->prev (coercion 'get type prev)))
      (cond ((not prev)
             (error 'project
                    "cannot project further ~a"
                    type))
            ((not type->prev)
             (error 'project
                    "cannot find coercion ~a->~a"
                    type prev)))
      (type->prev obj)))

(define (drop obj)
  (if (tower 'prev (type-tag obj))
    (let ((result (project obj)))
      (if (equ? result obj)
        (drop result) 
        obj))
    obj))
```

Давайте добавим методов:
```racket
(define (rational->integer r)
  (make-integer (quotient (numer r) 
                          (denom r))))
(define (real->rational r)
  (let ((r (contents r)))
    (cond ((eqv? +inf.0 r)
           (make-rational (expt 2 1024)
                          1))
          ((eqv? -inf.0 r)
           (make-rational (expt -2 1024)
                          1))
          ((eqv? +nan.0 r)
           (make-rational 0 1))
          ((and (exact? r)
                (rational? r))
           (make-rational 
             (numerator r)
             (denominator r)))
          (else 
           (let ((r (rationalize
                      (inexact->exact r)
                      1/10000)))
             (make-rational
               (numerator r)
               (denominator r)))))))
(define (complex->real c)
  (make-real (real-part c)))

(coercion 'put 'rational 'integer  rational->integer)
(coercion 'put 'real 'rational  real->rational)
(coercion 'put 'complex 'real  complex->real)
```

И потестируем:
```
> (drop (make-complex 1 0))
(integer . 1)
> (drop (make-complex 1.5 2.5))
(complex rectangular 1.5 . 2.5)
> (drop (make-complex 1.5 0.0))
(rational 3 . 2)
> (drop (make-complex 1.5358732589 0.0))
(real . 1.5358732589)
> (drop (attach-tag 'wonderful "wow!"))
; tower: type wonderful is not yet registred [,bt for context]
> (drop (make-rational 3 1))
(integer . 3)
> (drop (make-real 5.0))
(integer . 5)
```

Теперь добавим drop к apply-generic получив что-то вот такое:
```racket
(let ((result (apply-loop args)))
  (if (and (tagged? result)
           (tower 'in (type-tag result)))
    (drop result)
    result))
```

Мы проверяем является ли наш объектик тегнутым, и если да, то проверяем, лежит ли он в башне.
И если да, то дропаем.

А что касается предиката tagged?, его надо добавить, что-нибудь такое в нашем случае:
```racket
(define (tagged? datum)
  (and (pair? datum)
       (symbol? (car datum))))
```

И всё будет работать:
```
> (add (make-complex 1/3 2/3) (make-complex 3/5 -2/3))
(rational 14 . 15)
> (div (make-complex-from-mag-ang 5 3)
       (make-complex-from-mag-ang 1 3))
(integer . 5)
```

## 2.86

Чтобы всё работало нам нужны следующие изменения:
1. Новые generic операции
   ```racket
   (define (square-root x) (apply-generic 'square-root x))
   (define (sine x) (apply-generic 'sine x))
   (define (cosine x) (apply-generic 'cosine x))
   (define (arctan x y) (apply-generic 'arctan x y))
   ```
2. Мы можем поместить в пакет real эти операции. Вообще говоря интересно заметить, что единственную операцию, которую мы не делали с вами — это вычисление арктангенса. Всё остальное в рамках курса разбиралось.
   ```racket
   (generics 'put 'sine '(real) 
        (lambda (x) (tag (sin x))))
   (generics 'put 'cosine '(real)
        (lambda (x) (tag (cos x))))
   (generics 'put 'arctan '(real real)
        (lambda (x y) (tag (atan x y)))
   (generics 'put 'square-root '(real)
        (lambda (x) (tag (sqrt x))))
   ```
3. Поменять coercion в башне:
   ```racket
   (define (real->complex r)
     (make-complex (drop r) (make-integer 0)))
   (define (integer->real x)
     (rational->real (integer->rational x)))
   (coercion 'put 'integer 'real integer->real)

   (define (complex->real c)
     (let ((r (real-part c)))
       (cond ((eq? 'real (type-tag r))
              r)
             ((coercion 'get (type-tag r) 'real)
              ((coercion 'get (type-tag r) 'real) r))
             (else
              (error 'complex->real
                     "cannot coerce to real-part ~a of complex ~a"
                     r c)))))
   ```
4. Поменять пакеты polar, rectangular и complex.
   ```racket
   (define (install-rectangular-package)
     ;; internal procedures
     (define (square x) (mul x x))
     (define (real-part z) (car z))
     (define (imag-part z) (cdr z))
     (define (make-from-real-imag x y) 
       (cons x y))
     (define (magnitude z)
       (square-root (add (square (real-part z))
                         (square (imag-part z)))))
     (define (angle z)
       (arctan (imag-part z) (real-part z)))
     (define (make-from-mag-ang r a)
       (cons (mul r (cosine a)) (mul r (sine a))))
     ;; interface to the rest of the system
     (define (tag x) 
       (attach-tag 'rectangular x))
     (generics 'put 'real-part '(rectangular) real-part)
     (generics 'put 'imag-part '(rectangular) imag-part)
     (generics 'put 'magnitude '(rectangular) magnitude)
     (generics 'put 'angle '(rectangular) angle)
     (generics 'put 'make-from-real-imag 'rectangular
          (lambda (x y) 
            (tag (make-from-real-imag x y))))
     (generics 'put 'make-from-mag-ang 'rectangular
          (lambda (r a) 
            (tag (make-from-mag-ang r a))))
     'done)

   (define (install-polar-package)
     ;; internal procedures
     (define (square x) (mul x x))
     (define (magnitude z) (car z))
     (define (angle z) (cdr z))
     (define (make-from-mag-ang r a) (cons r a))
     (define (real-part z)
       (mul (magnitude z) (cosine (angle z))))
     (define (imag-part z)
       (mul (magnitude z) (sine (angle z))))
     (define (make-from-real-imag x y)
       (cons (square-root (add (square x) (square y)))
             (arctan y x)))
     ;; interface to the rest of the system
     (define (tag x) (attach-tag 'polar x))
     (generics 'put 'real-part '(polar) real-part)
     (generics 'put 'imag-part '(polar) imag-part)
     (generics 'put 'magnitude '(polar) magnitude)
     (generics 'put 'angle '(polar) angle)
     (generics 'put 'make-from-real-imag 'polar
          (lambda (x y) 
            (tag (make-from-real-imag x y))))
     (generics 'put 'make-from-mag-ang 'polar
          (lambda (r a) 
            (tag (make-from-mag-ang r a))))
     'done)

   (define (install-complex-package)
     ;; imported procedures from rectangular 
     ;; and polar packages
     (define (make-from-real-imag x y)
       ((generics 'get 'make-from-real-imag 
             'rectangular) 
        x y))
     (define (make-from-mag-ang r a)
       ((generics 'get 'make-from-mag-ang 'polar) 
        r a))
     ;; internal procedures
     (define (add-complex z1 z2)
       (make-from-real-imag 
        (add (real-part z1) (real-part z2))
        (add (imag-part z1) (imag-part z2))))
     (define (sub-complex z1 z2)
       (make-from-real-imag 
        (sub (real-part z1) (real-part z2))
        (sub (imag-part z1) (imag-part z2))))
     (define (mul-complex z1 z2)
       (make-from-mag-ang 
        (mul (magnitude z1) (magnitude z2))
        (add (angle z1) (angle z2))))
     (define (div-complex z1 z2)
       (make-from-mag-ang 
        (div (magnitude z1) (magnitude z2))
        (sub (angle z1) (angle z2))))
     (define (eq-complex? z1 z2)
       (and (equ? (real-part z1) (real-part z2))
            (equ? (imag-part z1) (imag-part z2))))
     (define (complex=zero? z1)
       (=zero? (magnitude z1)))

     ;; interface to rest of the system
     (define (tag z) (attach-tag 'complex z))
     (generics 'put 'add '(complex complex)
          (lambda (z1 z2) 
            (tag (add-complex z1 z2))))
     (generics 'put 'sub '(complex complex)
          (lambda (z1 z2) 
            (tag (sub-complex z1 z2))))
     (generics 'put 'mul '(complex complex)
          (lambda (z1 z2) 
            (tag (mul-complex z1 z2))))
     (generics 'put 'div '(complex complex)
          (lambda (z1 z2) 
            (tag (div-complex z1 z2))))
     (generics 'put 'equ? '(complex complex) eq-complex?)
     (generics 'put '=zero? '(complex) complex=zero?)
     (generics 'put 'make-from-real-imag 'complex
          (lambda (x y) 
            (tag (make-from-real-imag x y))))
     (generics 'put 'make-from-mag-ang 'complex
          (lambda (r a) 
            (tag (make-from-mag-ang r a))))
     (generics 'put 'real-part '(complex) real-part)
     (generics 'put 'imag-part '(complex) imag-part)
     (generics 'put 'magnitude '(complex) magnitude)
     (generics 'put 'angle '(complex) angle)

     'done)
   ```

И давайте немного проверим что получилось:
```
> (define z (make-complex (make-integer 3) (make-integer 4)))
> z
(complex rectangular (integer . 3) integer . 4)
> (magnitude z) 
(integer . 5)
> (angle z)
(real . 0.9272952180016122)
> (mul z z)
(complex polar (integer . 25) real . 1.8545904360032244)
> (magnitude (mul z z))
(integer . 25)
> (magnitude (make-integer 5))
(integer . 5)
> (mul (make-rational 5 6) (make-complex-from-mag-ang (make-integer 2) (make-rational 3 4)))
(complex polar (rational 5 . 3) rational 3 . 4)
> (add (make-complex (make-integer 1) (make-integer 2))
       (make-complex (make-real 1) (make-rational -2 1)))
(integer . 2)
> (=zero? (make-complex (make-integer 0) (make-real 0)))
#t
```

# 2.5.3 Symbolic Algebra

Я добавил новую generic операцию: repr.

```racket
(define (repr x) (apply-generic 'repr x))

(define (install-integer-package)
  ...
  (generics 'put 'repr '(integer) number->string)
  ...
  'done)

(define (install-rational-package)
  ...
  (define (repr r)
    (string-append 
      (number->string (numer r))
      "/"
      (number->string (denom r))))
  (generics 'put 'repr '(rational) repr)
  ...
  'done)

(define (install-real-package)
  ...
  (generics 'put 'repr '(real) number->string)
  ...
  'done)

(define (install-complex-package)
  ...
  (define (repr-complex c)
    (string-append 
      (repr (real-part c))
      "+"
      (repr (imag-part c))
      "i"))
  (generics 'put 'repr '(complex) repr-complex)
  ...
  'done)

(define (install-polynomial-package)
  ...
  (define (repr-term var term)
    (string-append 
      "[" (repr (coeff term)) "]" var "^" (number->string (order term))))
  (define (repr-termlist var terms)
    (cond ((empty-termlist? terms) 
           (string-append "[0]" var "^0"))
          ((empty-termlist? (rest-terms terms))
           (repr-term var (first-term terms)))
          (else
            (string-append 
              (repr-termlist var (rest-terms terms))
              " + "
              (repr-term var (first-term terms))))))
  (define (repr-poly p)
    (let ((var (symbol->string (variable p)))
          (terms (term-list p)))
      (repr-termlist var terms))) 
  (generics 'put 'repr '(polynomial) repr-poly)
```

Теперь например можно получать полиномчики:
```
> (define p1
    (make-polynomial 
      'x
      `((1 ,(make-rational 1 3))
        (2 ,(make-integer 1)))))
> (repr p1)
"[1]x^2 + [1/3]x^1"
```

## 2.87

Ну на данный момент =zero? это просто почти синоним empty-termlist?
Поэтому давайте добавим и всё у нас будет отлично работать.

```racket
(define (poly-zero? p)
  (empty-term-list? (term-list p)))

(generics 'put '=zero? '(polynomial) poly-zero?)
```

И какой-нибудь очевидный простенький тест:
```
> (define p1
    (make-polynomial 
      'x
      `((1 ,(make-rational 1 3))
        (2 ,(make-integer 1)))))

> (define p2
    (make-polynomial
      'x
      `((1 ,(make-rational -1 3))
        (2 ,(make-integer -1)))))

> (display (repr p1)) (newline)
[1]x^2 + [1/3]x^1
> (display (repr p2)) (newline)
[-1]x^2 + [-1/3]x^1
> (display (repr (add p1 p2))) (newline)
[0]x^0
> (=zero? (add p1 p2))
#t
```

## 2.88

Да, одна из проблем которая возникает при реализации вычитания — у нас может не быть того самого второго терма, чтобы его вычесть.
Поэтому нам надо как-то уметь отрицать термы. Но в таком случае можно упростить происходящее до негации полинома и суммы термов.

Короче давайте заведем negate для всех чисел и полиномов:

- в пакете **integer**:
  ```racket
  (generics 'put 'negate '(integer)
         (lambda (x) (tag (- x))))
  ```

- в пакете **rational**:
  ```racket
  (define (negate x)
    (make-rat (- (numer x))
              (denom x)))
  (generics 'put 'negate '(rational) 
       (lambda (x) (tag (negate x))))
  ```

- в пакете **real**:
  ``racket
  (generics 'put 'negate '(real)
         (lambda (x) (tag (- x))))
  ```

- в пакете **rectangular**:
  ```racket
  (define (negate-rectangular z)
    (make-from-real-imag (negate (real-part z))
                         (negate (imag-part z))))
  (generics 'put 'negate '(rectangular)
       (lambda (x) (tag (negate-rectangular x))))
  ```

- в пакете **polar**:
  ```racket
  (define (negate-polar x)
    (make-from-mag-ang
      (negate (magnitude x))
      (angle x)))
  (generics 'put 'negate '(polar)
       (lambda (x) (tag (negate-polar x))))
  ```

- в пакете **complex**:
  ```racket
  (generics 'put 'negate '(complex) 
       (lambda (z) (tag (negate z))))
  ```

- в пакете **polynomial**:
  ```racket
  (define (negate-termlist t)
    (if (empty-termlist? t)
      t
      (let* ((head (first-term t))
             (rest (rest-terms t))
             (ord (order head))
             (coe (coeff head)))
        (adjoin-term (make-term ord (negate coe))
                     (negate-termlist rest)))))
  (define (negate-poly p)
    (let ((v (variable p))
          (t (term-list p)))
      (make-poly v (negate-termlist t))))
  (generics 'put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  ```

И проверим что работает:
```
> (define p1
    (make-polynomial 
      'x `((1 ,(make-integer 1))
        (2 ,(make-rational 1 3))
        (3 ,(make-real 2.71828))
        (4 ,(make-complex-from-mag-ang (make-integer 1) 
                                       (make-integer 2)))
        (5 ,(make-complex (make-integer 1)
                          (make-integer 2)))
        (6 ,(make-polynomial 'y
                             `((0 ,(make-integer 1))
                               (1 ,(make-integer 2))))))))

> (define p2
    (negate p1))

> (display (repr p1)) (newline)
[[2]y^1 + [1]y^0]x^6 + [1+2i]x^5 + [-0.4161468365471424+0.9092974268256817i]x^4 + [2.71828]x^3 + [1/3]x^2 + [1]x^1
> (display (repr p2)) (newline)
[[-2]y^1 + [-1]y^0]x^6 + [-1+-2i]x^5 + [0.4161468365471424+-0.9092974268256817i]x^4 + [-2.71828]x^3 + [-1/3]x^2 + [-1]x^1
> (=zero? (add p1 p2)) 
#t
> (display (repr (negate (make-polynomial 'x '())))) (newline)
[0]x^0
```

Ну а теперь давайте сделаем sub для полиномов:

```racket
(define (sub-poly p1 p2)
  (if (same-variable? (variable p1)
                      (variable p2))
    (make-poly (variable p1)
               (add-terms (term-list p1)
                          (negate-termlist (term-list p2))))
    (error 'polynomial-package/sub
           "cannot sub polynomials, not in the same var ~a ~a"
           p1 p2)))

(generics 'put 'sub '(polynomial polynomial)
     (lambda (x y) (tag (sub-poly x y))))
```

```
> (define p1
    (make-polynomial 
      'x
      `((1 ,(make-integer 1))
        (6 ,(make-polynomial 'y
                             `((0 ,(make-integer 1))
                               (1 ,(make-integer 2))))))))
> (define p2
    (make-polynomial 
      'x
      `((1 ,(make-integer 4))
        (4 ,(make-polynomial 'y
                             `((1 ,(make-rational 1 2)))))
        (6 ,(make-polynomial 'y
                             `((0 ,(make-integer 1))
                               (1 ,(make-integer 2))))))))
> (display (repr p1)) (newline)
[[2]y^1 + [1]y^0]x^6 + [1]x^1
> (display (repr p2)) (newline)
[[2]y^1 + [1]y^0]x^6 + [[1/2]y^1]x^4 + [4]x^1
> (display (repr (sub p1 p2))) (newline)
[[-1/2]y^1]x^4 + [-3]x^1
```

## 2.89

Окей, прежде чем мы приступим к этой проблеме, я бы хотел подметить одну проблему: мы не умеем числа умножать на полиномы. Более того, мы не можем засунуть полином в башню типов, потому что не очевидно, до полинома по какой переменной мы будем поднимать наше число. И что это вообще значит? 

Почему я поднимаю эту проблему? Потому что при реализации dense полиномов у нас возникнет необходимость вставлять нули в термы. И к сожалению не ясно нули какого типа. Хотелось бы например просто нули.

В общем я не хочу, то, что называется загрязнять нашу систему, поэтому я предлагаю сделать большую переделку. Давайте у нас все числа, все операции над числами будут жить немножко отдельно от полиномов. Иначе говоря мы дадим числам собственный apply-generic, собственное всё. У нас будет просто тип number и будет просто тип polynomial, а так же мы вернем scheme-number.

В общем мы возьмём весь существующий код и закроем его в отдельный модуль чисел как таковых. Чтобы потом поверх этого сделать новую generic систему, которая с полиномами не пересекается.

Первое изменение которое мы сделаем это проведем локализацию таблиц:

Раньше например установка пакета выглядела вот так:
```racket
(define (install-integer-package)
  ...
  'done)
(install-integer-package)
```

И мы как бы дёргали имплицитно generics. Давайте теперь сделаем generics явным параметром.

```racket
(define (install-integer-package generics)
  ...
  'done)
(install-integer-package generics)
```

Теперь у нас есть возможность перенаправлять куда ползут вызовы. Проделаем это для каждой таблицы (полиномы временно удалим, чтобы не мешались). Получится что-то вот такое:

```racket
(install-integer-package generics)
(install-rational-package generics)
(install-real-package generics)
(install-rectangular-package generics)
(install-polar-package generics)
(install-complex-package generics)
(install-numerical-tower-package generics coercion tower)
```

Давайте проверим, что ничего не сломалось.

```
> (add (make-complex (make-integer 1) (make-integer -1)) 
       (make-complex (make-integer -1) (make-integer 1)))
(integer . 0)
```

So far so good. Теперь возьмём и всё засунем в пакет number
```racket
(define (install-number-package export-generics export-tag)
  (define (setup-type-tower)
    ;; код setup-type-tower
    ...)

  (let ((generics (setup-table 'generics))
        (coercion (setup-table 'coercion))
        (tower (setup-type-tower)))

    ;; абсолютно весь старый код (кроме setup-table)
    ...

    ;; installed packages
    (install-integer-package generics)
    (install-rational-package generics)
    (install-real-package generics)
    (install-rectangular-package generics)
    (install-polar-package generics)
    (install-complex-package generics)
    (install-numerical-tower-package generics coercion tower)

    ;; operations
    (export-generics 'put 'add '(number number)
           (lambda (x y)
             (export-tag 'number (add x y))))
    (export-generics 'put 'sub '(number number)
           (lambda (x y)
             (export-tag 'number (sub x y))))
    (export-generics 'put 'mul '(number number)
           (lambda (x y)
             (export-tag 'number (mul x y))))
    (export-generics 'put 'div '(number number)
           (lambda (x y)
             (export-tag 'number (div x y))))
    (export-generics 'put 'equ? '(number number)
           (lambda (x y)
             (equ? x y)))
    (export-generics 'put 'arctan '(number number)
           (lambda (x y)
             (export-tag 'number (arctan x y))))
    (export-generics 'put 'repr '(number)
           (lambda (x)
             (repr x)))
    (export-generics 'put 'negate '(number)
           (lambda (x)
             (export-tag 'number (negate x))))
    (export-generics 'put '=zero? '(number)
           (lambda (x)
             (=zero? x)))
    (export-generics 'put 'square-root '(number)
           (lambda (x)
             (export-tag 'number (square-root x))))
    (export-generics 'put 'sine '(number)
           (lambda (x)
             (export-tag 'number (sine x))))
    (export-generics 'put 'cosine '(number)
           (lambda (x)
             (export-tag 'number (cosine x))))
    (export-generics 'put 'numer '(number)
           (lambda (x)
             (numer x)))
    (export-generics 'put 'denom '(number)
           (lambda (x)
             (denom x)))
    (export-generics 'put 'real-part '(number)
           (lambda (x)
             (export-tag 'number (real-part x))))
    (export-generics 'put 'imag-part '(number)
           (lambda (x)
             (export-tag 'number (imag-part x))))
    (export-generics 'put 'magnitude '(number)
           (lambda (x)
             (export-tag 'number (magnitude x))))
    (export-generics 'put 'angle '(number)
           (lambda (x)
             (export-tag 'number (angle x))))

    ;; constructors 
    (export-generics 'put 'make-integer 'number
           (lambda (x)
             (export-tag 'number (make-integer x))))
    (export-generics 'put 'make-rational 'number
           (lambda (x y)
             (export-tag 'number (make-rational x y))))
    (export-generics 'put 'make-real 'number
           (lambda (x)
             (export-tag 'number (make-real x))))
    (export-generics 'put 'make-complex-from-real-imag 'number
           (lambda (x y)
             (export-tag 'number (make-complex-from-real-imag x y))))
    (export-generics 'put 'make-complex-from-mag-ang 'number
           (lambda (x y)
             (export-tag 'number (make-complex-from-mag-ang x y))))
    (export-generics 'put 'make-complex 'number
           (lambda (x y)
             (export-tag 'number (make-complex-from-real-imag x y)))))
  'done)
```

Теперь нам надо сделать подвязки в пакете сверху.

```racket
(define generics (setup-table 'generics))

  (define (tagged? datum)
    (and (pair? datum)
         (symbol? (car datum))))

  (define (attach-tag type-tag contents)
    (cons type-tag contents))
          
  (define (type-tag datum)
    (cond ((pair? datum)
           (car datum))
          (else 
           (error 'type-tag 
                  "bad tagged datum ~a" datum))))
      
  (define (contents datum)
    (cond ((pair? datum) 
           (cdr datum))
          (else 
           (error 'contents 
                  "bad tagged datum ~a" datum))))

  (define (apply-generic op . args)
    (define (tags args)
      (map type-tag args))
    (define (method-not-found)
      (error 'apply-generic
             "method not found ~a ~a"
             op (tags args)))
    (define (search args)
      (generics 'get op (tags args)))
    (define (has? args) 
      (generics 'has? op (tags args)))
    (define (apply-strip args)
      (apply (search args) (map contents args)))
    (cond ((has? args)
           (apply-strip args))
          (else 
            (method-not-found))))

(define (repr x) (apply-generic 'repr x))
(define (negate x) (apply-generic 'negate x))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (square-root x) (apply-generic 'square-root x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan x y) (apply-generic 'arctan x y))
(define (numer rat) (apply-generic 'numer rat))
(define (denom rat) (apply-generic 'denom rat))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-integer n)
  ((generics 'get 'make-integer 'number) n))
(define (make-rational n d)
  ((generics 'get 'make-rational 'number) n d))
(define (make-real n)
  ((generics 'get 'make-real 'number) n))

(define (make-complex-from-real-imag x y)
  ((generics 'get 'make-complex-from-real-imag 'number) (contents x) 
                                                        (contents y)))
(define (make-complex-from-mag-ang r a)
  ((generics 'get 'make-complex-from-mag-ang 'number) (contents r) 
                                                      (contents a)))
(define (make-complex x y)
  ((generics 'get 'make-complex 'number) (contents x) 
                                         (contents y)))

(define (intstall-number-package export-generics export-tag) ...)

(install-number-package generics attach-tag)
```

Кроме конструкторов, для которых пришлось поменять типы и пробросить контент, это практически копипаста.
Зато что мы получили? Теперь упростился apply-generic до своего стартового состояния, когда он не умел в coercion. И это замечательно, потому что теперь мы можем не думать что происходит в пакете number: там может происходить что угодно.

Давайте попробуем результаты наших трудов:
```
> (make-integer 1)
(number integer . 1)
> (add (make-integer 1) (make-integer 2))
(number integer . 3)
> (add (make-complex (make-integer 1) (make-integer 2))
       (make-complex (make-integer 1) (make-integer 2)))
(number complex rectangular (integer . 2) integer . 4)
> (mul (make-complex (make-integer 1) (make-integer 2))
       (make-complex (make-integer 1) (make-integer 2)))
(number complex polar (real . 5.000000000000001) real . 2.214297435588181)
> (cosine (make-real 1/3))
(number real . 0.9449569463147377)
> (display 
    (repr 
      (mul (make-complex (make-integer 1) (make-integer 2))
           (make-complex (make-integer 1) (make-integer 2))))) (newline)
-3+4.000000000000002i
> (=zero? (sub (make-integer 2) (make-rational 2 1)))
#t
> (equ? (make-integer 1) (make-complex (make-integer 1) (make-integer 0)))
#t
> (denom (make-integer 1))
(number . 1)
```

Ну в общем похоже на правду. Я тут напоролся на кучу ошибок, потому что копипаста немного зло, ну да ладно. Вы из не видите, потому что я их уже решил. Но может есть еще какие-то баги. Пора наверное заводить какие-то тесты... 

Но я пока всё еще не буду, несмотря на то, что программа уже стала довольно жирной.

Единственное, что мне выше не нравится, это то, что ```denom``` и ```numer``` возвращают не ```(number integer . 1)```. Это немножко наверное не ожидаемое поведение. Потому что мы наверняка ожидаем всё таки что-то такое.

Ну это исправляется исправлением пакета ```rational``` и ```coercion``` между ```integer->rational```, ```rational->integer```:

- в пакете **rational**:
  ```racket
  (generics 'put 'make 'rational
    (lambda (n d)
      (if (or (not (eq? (type-tag n) 'integer))
              (not (eq? (type-tag d) 'integer)))
          (error 'rational-package/make
                 "cannot create rational not from integers")
          (let ((n (integer->scheme-number n))
                (d (integer->scheme-number d)))
            (tag (make-rat n d))))))
  (generics 'put 'numer '(rational) 
            (lambda (x) 
              (make-integer (numer x))))
  (generics 'put 'denom '(rational) 
            (lambda (x) 
              (make-integer (denom x))))
  ```

- в пакете **numerical-tower**:
  ```racket
  (define (integer->rational i)
    (make-rational i
                   (make-integer 1)))
  (define (rational->real r)
    (make-real (/ (integer->scheme-number (numer r)) 
                  (integer->scheme-number (denom r)))))
  (define (rational->integer r)
    (numer r))
  (define (real->rational r)
    (let ((r (contents r)))
      (cond ((eqv? +inf.0 r)
             (make-rational (make-integer (expt 2 1024))
                            (make-integer 1)))
            ((eqv? -inf.0 r)
             (make-rational (make-integer (expt -2 1024))
                            (make-integer 1)))
            ((eqv? +nan.0 r)
             (make-rational (make-integer 0) 
                            (make-integer 1)))
            ((and (exact? r)
                  (rational? r))
             (make-rational 
               (make-integer (numerator r))
               (make-integer (denominator r))))
            (else 
             (let ((r (rationalize
                        (inexact->exact r)
                        1/10000)))
               (make-rational
                 (make-integer (numerator r))
                 (make-integer (denominator r))))))))
  ```

Наконец, нам надо предоставить integer->scheme-number, мы сделаем его так (внутри пакета **number**):
```racket
(define (integer->scheme-number i)
  (contents i))
```

И давайте перепишем конструкторы, как по мне они должны стать обычными методами вроде:
```racket
;; constructors 
(export-generics 'put 'make-integer '(scheme-number)
       (lambda (x)
         (export-tag 'number (make-integer x))))
(export-generics 'put 'make-rational '(number number)
       (lambda (x y)
         (export-tag 'number (make-rational x y))))
(export-generics 'put 'make-real '(scheme-number)
       (lambda (x)
         (export-tag 'number (make-real x))))
(export-generics 'put 'make-complex-from-real-imag '(number number)
       (lambda (x y)
         (export-tag 'number (make-complex-from-real-imag x y))))
(export-generics 'put 'make-complex-from-mag-ang '(number number)
       (lambda (x y)
         (export-tag 'number (make-complex-from-mag-ang x y))))
(export-generics 'put 'make-complex '(number number)
       (lambda (x y)
         (export-tag 'number (make-complex-from-real-imag x y)))))
```

И обвязки:
```racket
(define (make-integer n) (apply-generic 'make-integer n))
(define (make-rational n d) (apply-generic 'make-rational n d))
(define (make-real n) (apply-generic 'make-real n))

(define (make-complex-from-real-imag x y) 
  (apply-generic 'make-complex-from-real-imag x y))
(define (make-complex-from-mag-ang r a)
  (apply-generic 'make-complex-from-mag-ang r a))
(define (make-complex x y)
  (apply-generic 'make-complex x y))
```

Единственная проблема: нам нужен scheme-number!
Давайте его наконец вернем. И может быть как и раньше сделаем его прозрачным.

```racket
(define (make-scheme-number x)
  ((generics 'get 'make 'scheme-number) x))

(define (install-scheme-number-package generics export-tag)
    (define (tag x)
      (export-tag 'scheme-number x))
    (define (make x)
      (if (not (number? x))
        (error 'scheme-number-package/make
               "expected number but got ~a"
               x))
        (tag x))
    (generics 'put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (generics 'put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (generics 'put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (generics 'put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (generics 'put 'negate '(scheme-number)
         (lambda (x) (tag (- x))))
    (generics 'put 'sine '(scheme-number) 
         (lambda (x) (tag (sin x))))
    (generics 'put 'cosine '(scheme-number)
         (lambda (x) (tag (cos x))))
    (generics 'put 'arctan '(scheme-number scheme-number) 
         (lambda (x y) (tag (atan x y))))
    (generics 'put 'square-root '(scheme-number)
         (lambda (x) (tag (sqrt x))))

    (generics 'put 'equ? '(scheme-number scheme-number) =)
    (generics 'put 'repr '(scheme-number) number->string)
    (generics 'put '=zero? '(scheme-number)
         (lambda (x) (= x 0)))
    (generics 'put 'make 'scheme-number make)
    'done)

(install-scheme-number-package generics attach-tag)
```

Давайте опробуем работу:
```
> (add (make-integer (make-scheme-number 1))
       (make-real (make-scheme-number 2.4643)))
(number real . 3.4643)
```

Давайте теперь вернем старые добрые приколы в стиле того, что scheme-number нашей системой распознается автоматически: нам не надо вызывать конструктор.

```racket
(define (tagged? datum)
  (or (number? datum)
      (and (pair? datum)
           (symbol? (car datum)))))

(define (attach-tag type-tag contents)
  (cond ((and (eq? type-tag 'scheme-number)
              (number? contents))
         contents)
        ((and (eq? type-tag 'scheme-number)
              (not (number? contents)))
         (error 'attach-tag
                "trying to attach scheme-number tag to ~a"
                contents))
        (else 
          (cons type-tag contents))))
        
(define (type-tag datum)
  (cond ((number? datum)
         'scheme-number)
        ((pair? datum)
         (car datum))
        (else 
         (error 'type-tag 
                "bad tagged datum ~a" datum))))
    
(define (contents datum)
  (cond ((number? datum)
         datum)
        ((pair? datum) 
         (cdr datum))
        (else 
         (error 'contents 
                "bad tagged datum ~a" datum))))
```

И всё замечательно теперь работает:
```
> (make-integer 1)
(number integer . 1)
> (div (make-integer 1) (make-integer 2)) 
(number rational 1 . 2)
> (add (make-integer 1) (make-complex (make-real 1/2) (make-real 0)))
(number rational 3 . 2)
> (make-rational (make-integer 1) (make-real 1/2))
rational-package/make: cannot create rational not from integers [,bt for context]
> (repr (make-rational (make-integer 1) (make-integer 1)))
"1/1"
```

Возможно стоит еще добавить вещь которую я в прошлый раз не додумался добавить: перед тем как делать raise, хорошо бы сделать всем аргументам, которые того заслуживают: drop. Давайте добавим это поведение.

```racket
(define (apply-generic op . args)
  ...
  (define (drop-if-can arg)
    (if (and (pair? arg)
             (tower 'in (type-tag arg)))
      (drop arg)
      arg))

  (define (raise-loop args)
    (cond ((can-be-raised? args)
           (raise-loop (raise-all args)))
          (else
            (method-not-found))))

  (define (apply-loop args)
    (if (has? args)
      (apply-strip args)
      (let ((dropped (map drop-if-can args)))
        (raise-loop dropped))))
  
  (drop-if-can (apply-loop (map drop-if-can args))))
```

И надо правда немного переписать дроп, вот с этого:
```racket
(define (drop obj)
  (if (tower 'prev (type-tag obj))
    (let ((result (project obj)))
      (if (equ? result obj)
        (drop result) 
        obj))
    obj))
```

На тот который не циклится:
```racket
(define (drop obj)
  (if (tower 'prev (type-tag obj))
    (let ((result (project obj)))
      (if (equ? (raise result) obj)
        (drop result) 
        obj))
    obj))
```

Проблема в том что equ? — это generic операция, и она у нас пойдёт сравниваться. Не найдёт себя, пойдёт дропнет аргументы. А раз мы её вызвали из дропа, то мы снова пойдём в ```equ?```, мы снова попытаемся их дропнуть. И т.д. Короче бесконечный цикл.

Для пущей безопасности надо вообще убрать всю эту чушь и вызвать equ от соответствующего типа напрямую, а не через наши цыганские фокусы. Но постольку поскольку ```equ?``` определен для каждого числа из башни, и постольку поскольку трансформеры ```a->b``` корректны, например ```integer->rational``` действительно переводит целые в рациональные — мы более менее в безопасности.

Если честно мне конечно то что мы накуролесили не нравится, потому что система очень хрупкая на самом деле.
Возможно надо как-то всё переписать, добавив всякие дополнительные штуки типа ```safe-numer```, который только рациональные числа ожидает, аналогично ```safe-equ?``` и прочие селекторы. Чтобы они не вздумали ничего ни дропать, ни поднимать.

Но я этого делать не буду, потому что всё и так разбор крайне затягивается.


Короче, ладно, хорошие новости: теперь у нас например работает ```numer``` и ```denom``` на изначально не рациональных числах!

```racket
> (denom (make-rational (make-integer 1) (make-integer 2)))
(number integer . 2)
> (denom (make-real 1/2))
(number integer . 2)
> (denom (make-complex (make-real 1/2) (make-integer 0)))
(number integer . 2)
> (denom (make-complex (make-real 1/2) (make-integer 1)))
apply-generic: method not found denom (complex) [,bt for context]
> (cosine (make-complex (make-real 1/2) (make-integer 0)))
(number real . 0.8775825618903728)
> (cosine (make-complex (make-real 1/2) (make-integer 1)))
apply-generic: method not found cosine (complex) [,bt for context]
```

И на этом наверное со всеми этими бесконечными числами покончено: они теперь живут в своей реальности, и нам не мешают.
Теперь мы можем полностью новые правила вводить, строить новые башни, третируя числа как просто навсего некий обобщённый типа ```number```.

Давайте теперь добавим coercion в нашу систему. 
А конкретно будем приводить ```scheme-number->number```:

Для этого нам надо будет добавить к пакету **scheme-number** соответствующие биндинги:
```racket
(define real-part 
  (eval 'real-part (scheme-report-environment 5)))
(define imag-part 
  (eval 'imag-part (scheme-report-environment 5))) 

(generics 'put 'real-part '(scheme-number)
     (lambda (x) (tag (real-part x))))
(generics 'put 'imag-part '(scheme-number)
     (lambda (x) (tag (imag-part x))))
(generics 'put 'numer '(scheme-number)
     (lambda (x) (tag (numerator x))))
(generics 'put 'denom '(scheme-number)
     (lambda (x) (tag (denominator x))))
```

Это нам надо сделать потому что у нас к сожалению есть проблема в виде generic операций numer, denom, real-part, imag-part.

Зато теперь можно сделать достаточно тривиальную конверсию:
```racket
(define (scheme-number->number x)
  (cond ((integer? x)
         (make-integer x))
        ((and (exact? x)
              (rational? x))
         (make-rational (make-integer (numer x))
                        (make-integer (denom x))))
        ((real? x)
         (make-real x))
        ((complex? x)
         (make-complex (make-real (real-part x))
                       (make-real (imag-part x))))))
```

```
> (scheme-number->number 1)
(number integer . 1)
> (scheme-number->number 1/2)
(number rational 1 . 2)
> (scheme-number->number 1.24)
(number real . 1.24)
> (scheme-number->number 1+2i)
(number complex rectangular (real . 1) real . 2)
```

Давайте наконец вернем наши полиномы:
```
(define (install-polynomial-package generics attach-tag)
  ...)
(install-polynomial-package generics attach-tag)

(define (scheme-number->polynomial var x)
  (make-polynomial var `((0 ,x))))
(define (number->polynomial var x)
  (make-polynomial var `((0 ,x))))
```

И сделаем generic операцию ```juggle```, которая будет список аргументов определенным образом колдовать. Ну что-то в стиле: 
```racket
(define juggle-table (setup-table 'juggle))

(define (juggle . args)
  (define (tags args)
    (map type-tag args))
  (let ((proc (juggle-table 'get 'juggle (tags args))))
    (if proc
      (apply proc args)
    false)))

(define (install-juggle-package juggle)
  (juggle 'put 'juggle '(scheme-number number)
    (lambda (a b)
      (list (scheme-number->number a) b)))
  (juggle 'put 'juggle '(number scheme-number)
    (lambda (a b)
      (list a (scheme-number->number b))))
  (juggle 'put 'juggle '(polynomial scheme-number)
    (lambda (a b)
      (list a (scheme-number->polynomial (variable a) b))))
  (juggle 'put 'juggle '(scheme-number polynomial)
    (lambda (a b)
      (list (scheme-number->polynomial (variable b) a) b)))
  (juggle 'put 'juggle '(polynomial number)
    (lambda (a b)
      (list a (number->polynomial (variable a) b))))
  (juggle 'put 'juggle '(number polynomial)
    (lambda (a b)
      (list (number->polynomial (variable b) a) b)))
  'done)
```

Ну чтобы всё работало, надо еще достать variable из полиномов (в принципе почему бы и нет)
```racket
(define (install-polynomial-package ...)
  ...
  (generics 'put 'variable '(polynomial)
       (lambda (p) (variable p)))
  ...)

(define (variable p) (apply-generic 'variable p))
```


Короче говоря посмотрите и узрите:
```
> (juggle (make-polynomial 'x '((0 1) (1 2))) 1)
((polynomial x (0 1) (1 2)) (polynomial x (0 1))) 
> (juggle (make-polynomial 'y '((0 1) (1 2))) 1)
((polynomial y (0 1) (1 2)) (polynomial y (0 1)))
> (juggle 1 (make-complex (make-integer 1) (make-real 2)))
((number integer . 1) (number complex rectangular (integer . 1) real . 2))
> (juggle (make-polynomial 'x '((0 1) (1 2))) (make-integer 0))
((polynomial x (0 1) (1 2)) (polynomial x (0 (number integer . 0))))
```

Давайте теперь добавим эти правила в ```apply-generic```:
```racket
(define (apply-generic op . args)
  ...
  (if (has? args)
      (apply-strip args)
      (let ((juggled (apply juggle args)))
        (if (and juggled
                 (has? juggled))
          (apply-strip juggled)
          (method-not-found)))))
```

И теперь у нас более менее работают (наконец-то!) всякие конверсии автоматически:
```
> (add (make-polynomial 'x '((0 1) (1 2))) (make-integer 0))
(polynomial x (0 (number integer . 1)) (1 2))
> (repr (add (make-polynomial 'x '((0 1) (1 2))) (make-integer 0)))
"[2]x^1 + [1]x^0"
> (repr (mul (make-polynomial 'x '((0 1) (1 2))) 0))
"[0]x^0"
> (repr (mul (make-polynomial 'y '((0 1) (1 2))) 0))
"[0]y^0"
> (define p1
    (make-polynomial 
      'x
      `((1 ,(make-integer 1))
        (6 ,(make-polynomial 'y
                             `((0 ,(make-integer 1))
                               (1 ,(make-integer 2))))))))
> (define p2
    (make-polynomial 
      'x
      `((1 ,(make-integer 4))
        (4 ,(make-polynomial 'y
                             `((1 ,(make-rational (make-integer 1) 
                                                  (make-integer 2))))))
        (6 ,(make-polynomial 'y
                             `((0 ,(make-integer 1))
                               (1 ,(make-integer 2))))))))
> (repr p1)
"[[2]y^1 + [1]y^0]x^6 + [1]x^1"
> (repr p2)
"[[2]y^1 + [1]y^0]x^6 + [[1/2]y^1]x^4 + [4]x^1"
> (repr (mul p1 p2))
"[[4]y^2 + [4]y^1 + [1]y^0]x^12 + [[1]y^2 + [1/2]y^1]x^10 + [[10]y^1 + [5]y^0]x^7 + [[1/2]y^1]x^5 + [4]x^2"
```

Теперь мы можем перейти непосредственно к заданию, которое нас просили. А просили нас следующее:
> Define procedures that implement the term-list representation described above as appropriate for dense polynomials. 

Мы попытаемся поменять polynomial package минимальным образом. Для начала изменить конструктор ```make-poly```:
```racket
(define (make-poly variable term-list)
  (let ((term-list (map (lambda (t) 
                          (apply make-term t))
                        term-list)))
    (cons variable
          (foldr (the-empty-termlist) 
                 (lambda (term-list term)
                   (adjoin-term term term-list))
                 term-list))))
```

Кратко, мы сделали так, что теперь репрезентация термов не зависит от конструктора. 
Он просто сам берет и собирает.

Давайте проверим что ничего не сломалось:
```

```
> (define p2
    (make-polynomial 
      'x
      `((6 ,(make-polynomial 'y
                             `((0 ,(make-integer 1))
                               (1 ,(make-integer 2)))))
        (1 ,(make-integer 4))
        (4 ,(make-polynomial 'y
                             `((1 ,(make-rational (make-integer 1) 
                                                  (make-integer 2)))))))))
> (repr p2)
"[4]x^1 + [[1/2]y^1]x^4 + [[1]y^0 + [2]y^1]x^6"
```

Давайте модифицируем старый adjoin-term, чтобы он игнорировал порядок.
```racket
(define (adjoin-term term term-list)
  (cond ((=zero? (coeff term)) 
         term-list)
        ((empty-termlist? term-list)
         (cons term term-list))
        ((order< term (first-term term-list)) 
         (cons term term-list))
        ((order> term (first-term term-list))
         (cons (first-term term-list)
               (adjoin-term term (rest-terms term-list))))
        ((order= term (first-term term-list))
         (error 'adjoin-term
                "term with order ~a already exists"
                (order term)))))
```

Проверяем:
```
> (define p2
    (make-polynomial 
      'x
      `((6 ,(make-polynomial 'y
                             `((0 ,(make-integer 1))
                               (1 ,(make-integer 2)))))
        (1 ,(make-integer 4))
        (4 ,(make-polynomial 'y
                             `((1 ,(make-rational (make-integer 1) 
                                                  (make-integer 2)))))))))
> (repr p2)
"[4]x^1 + [[1/2]y^1]x^4 + [[1]y^0 + [2]y^1]x^6"


> (make-polynomial 'x '((0 1) (0 1)))
adjoin-term: term with order 0 already exists [,bt for context]
```

Альтернативно конечно можно складывать термы. Но я боюсь это немножко плохая идея.
Всё таки ajoin-term используется в первую очередь для модификации полиномов.

Всё! Сохранимся на этой точке (потому что в будущем мы будем объединять обе версии полиномов).
И перейдём к рассмотрению dense полиномов наконец.


