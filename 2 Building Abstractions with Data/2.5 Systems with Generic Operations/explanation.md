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

Ну смотрите, что делает magnitude? magnitude достаёт тип комплексного числа, по нему ищет соответствующую операцию, выбрасывает тип и делегирует найдённой операции задачу. 

Когда мы вызываем magnitude на типе complex, то происходит делегация ни к чему иному как к magnitude в очередной раз. А так как в этот раз это уже комплексное число определенного типа (polar или rectangle), то всё получается.

На конец нам предлагают потрейсить вызовы apply-generic. Для этого мы воспользуемся стандартным racket/trace

```
> (trace apply-generic)
> (magnitude (make-complex-from-mag-ang 1 2))
>{apply-generic magnitude (complex polar 1 . 2)}
>{apply-generic magnitude (polar 1 . 2)}
<1
1
```

И мы видим, что вызывается оно ровно два раза, как мы это и предсказали выше.
В первом случае вызывается вновь magnitude, а во втором уже внутренняя реализация magnitude-polar или что-то на вроде этого.

## 2.78

Нас просят так видоизменить нашу программу, чтобы сложение чисел было прозрачным, заместо ```(add (make-scheme-number 1) (make-scheme-number 1))```, чтобы было ```(add 1 2)```.

Для этого нам надо видоизменить поведение attach-type-tag, type-tag и contents соответственно.

Для этого мы теперь запретим не-числам ставить тайптег scheme-number, а сами числа видоизменять не будем.
type-tag и contents теперь тоже пусть просто проверят, что нам поступило число и выкинут его или выкинут его тип: scheme-number, в зависимости от того, что мы вызвали.

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

Там дальше немного про то как они себя вести должны, но в целом строгих гарантий у нас... наверное нет.

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

Сперва добавим немножко новых методов: get-coercion, put-coercion.

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
    (if (generics 'get key-1 key-2)
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

Мы можем теперь добавить соответсвующий код, чтобы наши примеры работали из коробки.
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

Но я лично предпочитаю немножко переписать код из упражнений, потому что мне нравится самодокументируемость того, что я вызываю put и get из таблицы названной generics.

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

На самом деле пассаж выше это ответ на **второй** вопрос, потому что мы, давайте я сокращу, обозначили проблему: у нас случается некоректное поведение программы, если какой-то разработчик добавит приведение типа к самому себе.

И решением **третьего** пунка будет следующий код:
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

И у нас небольшая проблема: scheme по умолчанию идёт уже с этими типами данных. Причём между ними происходится всякая нехорошая конверсия в духе того, что мы хотим установить. Поэтому можно получить какие-то рандомные неожиданные баги.

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

Другой подход мог бы заключаться в том, чтобы вообще не предоставлять подобный метод (ну а зачем в самом деле). И заместо этого позволить apply-generic найти соответствующий метод у rational, привести integer->rational и произвести деление уже там. Давайте это сейчас запомним и в следующий раз протестируем.

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

rational это всё тот же пакет, за исключением может быть конструктора, но я не буду как-то ограничивать на самом деле входные данные, пофигу

real это по-сути переименнованный scheme-number, за исключением make, который тоже теперь должен выбрасывать ошибку:
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

и complex это всё тот же пакет, плюс я добавлю: 
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
