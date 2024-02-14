# 2.4.3 Data-Directed Programming and Additivity

Сначала, прежде чем мы будем выполнять задания, нам надо как-то реализовать ```(get key-1 key-2)``` и ```(put key-1 key-2 value)``` соответственно.

Потому что книга подразумевает, что оно у нас есть, а его у нас нет:
```
racket -I sicp
Welcome to Racket v8.2 [cs].
> put
; put: undefined;
;  cannot reference an identifier before its definition
;   in module: top-level
; [,bt for context]
```

Ждать следующей главы мы не можем, поэтому мы просто импортируем что-нибудь из racket.
```racket
#lang sicp

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

Теперь можно потестить например код, который нам предлагает книга (речь про комплексные числа).
```racket
#lang sicp

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

(define (square x)
  (* x x))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error 'type-tag 
             "bad tagged datum ~a" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error 'contents 
             "bad tagged datum ~a" datum)))

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (apply (get op type-tags) (map contents args))))

(install-polar-package)
(install-rectangular-package)

(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 
        'rectangular) 
   x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 
        'polar) 
   r a))

(define (add-complex z1 z2)
  (make-from-real-imag 
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag 
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang 
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang 
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))
```

Заметьте что я немного поменял интерфейс, поэтому и ```apply-generic```чуть упростился.
Теперь мы получаем содержательные ошибки от самого get. А в случае если нам всё же потребуется как-то узнать без ошибки, есть ли метод, мы всегда можем дёрнуть ```has?```.

```
done
done
> (mul-complex (make-from-real-imag 1 2) 
               (make-from-real-imag 3 4))
(polar 11.180339887498949 . 2.0344439357957027)
> (add-complex (make-from-real-imag 0 0)
               (mul-complex (make-from-real-imag 1 2) 
                            (make-from-real-imag 3 4)))
(rectangular -5.0 . 10.0)
```

Ну отлично, то, что ожидалось.

## 2.73

1. Произошло следующее: мы теперь делаем диспатч по типу операции, заместо того, чтобы руками прописывать правила для каждой конкретной операции. 
   
   Мы не можем не проверить expr на number? variable? или compound?, по той причине что на вход нам поступает не-типизированное выражение. А вот проблем с разными операциями не возникает, потому что в качестве типа операции мы можем взять просто оператор каждой операции (при условии конечно, что они имеют синтаксис ```(op args)```).

2. Ну сначала проверим что всё работает как заявлено:
   ```
   > (deriv 1 'x)
   0
   > (deriv 'x 'x)
   1
   > (deriv '(+ x y) 'x)
   ; get: method deriv for type + does not exist [,bt for context]
   ```
   
   Теперь добавим наши пакеты:
   ```racket
   (define (make op)
     (lambda (operands)
       (cons op operands)))

   (define (install-sum-package)
     (define (deriv-sum ops var) 
       (define (deriv- x) (deriv x var))
       ((make '+) 
        (map deriv- ops)))

     (put 'deriv '+ deriv-sum) 
     'done)

   (define (install-product-package)
     (define (clauses args var)
       (if (null? args)
         '()
         (let ((head (car args))
               (rest (cdr args)))
           (cons (cons (deriv head var) rest)
                 (map (lambda (x)
                        (cons head x))
                      (clauses rest var))))))

     (define (deriv-product ops var)
       ((make '+) 
        (map (make '*)                             
             (clauses ops var))))  

     (put 'deriv '* deriv-product) 
     'done)

   (install-product-package)
   (install-sum-package)
   ```
   
   И теперь уже всё получается:
   ```
   > (deriv '(+ x y) 'y)
   (+ 0 1)
   > (deriv '(* x x) 'x)
   (+ (* 1 x) (* x 1))
   ```
3. Давайте для разнообразия добавим что-нибудь новенькое, например -, sin и cos.
   
   ```racket 
   (define (install-sub-package)
     (define (deriv-sub ops var) 
       (define (deriv- x) (deriv x var))
       ((make '-) 
        (map deriv- ops)))

     (put 'deriv '- deriv-sub) 
     'done)

   (define (install-trig-package)
     (define (deriv-sin ops var)
       (if (not (= 1 (length ops)))
         (error 'deriv
                "sin expected 1 argument ~a"
                ops))
       ((make '*) 
        (list ((make 'cos) ops)
              (deriv (car ops) var))))

     (define (deriv-cos ops var)
       (if (not (= 1 (length ops)))
         (error 'deriv
                "cos expected 1 argument ~a"
                ops))
       ((make '*) 
        (list ((make '-) 
               (list ((make 'sin) ops)))
              (deriv (car ops) var))))

     (put 'deriv 'sin deriv-sin)
     (put 'deriv 'cos deriv-cos)
     'done)
   ```

   И оно действительно (после установки пакетов) теперь работает:
   ```
   > (deriv '(sin (* x x)) 'x)
   (* (cos (* x x)) (+ (* 1 x) (* x 1)))
   > (deriv '(cos (* x x)) 'x)
   (* (- (sin (* x x))) (+ (* 1 x) (* x 1)))
   ```
4. Так как мы поменяли оператор и 'deriv, придётся в каждом пакете сделать то же самое в соответствующих вызовах put: 
  ```
  (put 'deriv '+ deriv-sum) -> (put '+ 'deriv deriv-sum)
  (put 'deriv '* deriv-product) -> (put '* 'deriv deriv-product)      
  (put 'deriv '- deriv-sub) -> (put '- 'deriv deriv-sub) 
  (put 'deriv 'sin deriv-sin) -> (put 'sin 'deriv deriv-sin)
  (put 'deriv 'cos deriv-cos) -> (put 'cos 'deriv deriv-cos)
  ```

  Что на самом деле сравнительно небольшое изменение.

```racket
#lang sicp

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

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error 'type-tag 
             "bad tagged datum ~a" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error 'contents 
             "bad tagged datum ~a" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (apply (get op type-tags) (map contents args))))

(define variable? symbol?)

(define (same-variable? v1 v2) 
  ;Are v1 and v2 the same variable?
  (and (variable? v1) (variable? v2)
       (eq? v1 v2)))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (make op)
  (lambda (operands)
    (cons op operands)))

(define (install-sum-package)
  (define (deriv-sum ops var) 
    (define (deriv- x) (deriv x var))
    ((make '+) 
     (map deriv- ops)))

  (put 'deriv '+ deriv-sum) 
  'done)

(define (install-product-package)
  (define (clauses args var)
    (if (null? args)
      '()
      (let ((head (car args))
            (rest (cdr args)))
        (cons (cons (deriv head var) rest)
              (map (lambda (x)
                     (cons head x))
                   (clauses rest var))))))

  (define (deriv-product ops var)
    ((make '+) 
     (map (make '*)                             
          (clauses ops var))))  

  (put 'deriv '* deriv-product)      
  'done)

(define (install-sub-package)
  (define (deriv-sub ops var) 
    (define (deriv- x) (deriv x var))
    ((make '-) 
     (map deriv- ops)))

  (put 'deriv '- deriv-sub) 
  'done)

(define (install-trig-package)
  (define (deriv-sin ops var)
    (if (not (= 1 (length ops)))
      (error 'deriv
             "sin expected 1 argument ~a"
             ops))
    ((make '*) 
     (list ((make 'cos) ops)
           (deriv (car ops) var))))

  (define (deriv-cos ops var)
    (if (not (= 1 (length ops)))
      (error 'deriv
             "cos expected 1 argument ~a"
             ops))
    ((make '*) 
     (list ((make '-) 
            (list ((make 'sin) ops)))
           (deriv (car ops) var))))

  (put 'deriv 'sin deriv-sin)
  (put 'deriv 'cos deriv-cos)
  'done)

(install-product-package)
(install-sum-package)
(install-sub-package)
(install-trig-package)
```

## 2.74

Итак, нас просят сделать систему управления всякими базюками над кучей компаний одновременно, каждая из которых имеет собственные представления о том как структурировать информацию.

Для этого мы каждую "базу" данных пометим соответствующим тайп-тегом конкретной дивизии и дальше будем реализовывать соответствующие интерфейсы.

1. Мы попросили выше, что файл каждой дивизии помечен тайп-тегом, поэтому мы просто сгоняем по ```(get 'get-record type-tag)``` и получим оттуда метод, который будет работать над нетипизированной базой, который будет доставать запись по имени сотрудника. Заметим, что каждая дочерняя компания может как угодно структурировать базу данных: это совершенно не имеет никакого значения.

   ```racket
   (define (get-record employee file)
     (let* ((division (type-tag file))
            (records (contents file))
            (get-record* (get 'get-record division))
            (record (get-record* employee records)))
       (if record 
         (attach-tag division record)
         record)))
   ```

   После чего мы сверху докинем тайп-тег на запись, чтобы она стала типизированная (ведь потом мы будем из неё доставать данные и надо как-то знать из какой дивизии её достали, и возвратим значение.

2. Ну здесь практически то же самое, что и в первом пункте, за исключением того, что мы в конце не будем пробрасывать тайп-тег. Так как на record у нас висит type-tag конкретного подразделения, то нам достаточно дёргать generic метод и совершенно не знать как устроены внутренности record.
   ```racket
   (define (get-salary record)
     (let* ((division (type-tag record))
            (data (contents record))
            (get-salary* (get 'get-salary division)))
         (get-salary* data)))
   ```
3. Теперь нам надо из предыдущих примитивов сколотить ```find-employee-record```, который по набору баз будет искать сотрудника и видимо возвращать результаты всех мест где сотрудник устроен (почему бы ему не работать сразу в двух местах).
   ```racket
   (define (find-employee-record employee files)
     (apply append 
            (map (lambda (file)                             
                   (let ((result (get-record employee file)))
                     (if result (list result) '())))
                 files)))
   ```
4. Ну, со стороны интерфейса управления вообще никаких изменений не произойдёт.
   Незначительные изменения произойдут только у нового подразделения, а конкретно ему нужно будет предоставить соответствующие реализации операций вроде ```get-record```, ```get-salary```. И еще нужно будет предоставить какой-то способ получить собственно файл.

Давайте для примера сделаем пару базюк и проверим что всё работает. Для этого я правда сперва добавлю еще пару generic методов, которые будет доставать базу данных по имени дивизии.

```racket
(define (load-file division)
  (attach-tag division ((get 'load-file division))))

(define (unload-file file)
  ((get 'unload-file (type-tag file)) (contents file)))
```

### "NaturalSchemes" [Scheme]

Сперва сделаем какую-нибудь простую вещь, а конкретно: прочитаем из файла простенькую таблицу сотрудников, что-то в стиле:
```racket
("Громов Михаил Константинович"       42037)
("Наумов Глеб Германович"             57490)
("Наумова Дарья Александровна"        34099)
("Фролов Никита Маркович"             52355)
("Савина Анна Егоровна"               42515)
("Юдин Михаил Тимофеевич"            124414)
("Сорокин Константин Константинович"  41444)
("Колесов Александр Савельевич"       87464)
("Горохов Макар Робертович"           52319)
("Сидоров Тимур Михайлович"           33244)
```

Для этого сперва поправим наши импорты, потому что нам необходимо много вещей сделать:
```racket
(#%require racket/path)
(#%require racket/runtime-path)
(#%require racket/mpair)
(#%require (only racket
                 void
                 build-path
                 file->list
                 make-hash
                 hash-set!
                 hash-ref))

(define-runtime-path *runtime-path* "2.74.rkt")
(define *runtime-dir* (path-only *runtime-path*))
```

И теперь можно реализовать наш сценарий:
```racket
(define (install-natural-schemes-package)
  (define dbpath
    (build-path *runtime-dir*                
                "assets/NaturalSchemes.list"))

  (define (convert lst)
    (map list->mlist (list->mlist lst)))

  (define (load-file)
    (convert (file->list dbpath)))

  (define (unload-file file)
    (void))

  (define get-record assoc)
  (define get-salary cadr)

  (put 'load-file 'natural-schemes load-file)
  (put 'unload-file 'natural-schemes unload-file)
  (put 'get-record 'natural-schemes get-record)
  (put 'get-salary 'natural-schemes get-salary)

  'done)

(install-natural-schemes-package)
```

Кратко: я пользуюсь встроенными штуками racket, чтобы найти где мы исполняемся, а затем найти в папке рядом со скриптом файл NaturalSchemes.list, который затем превращаю в список из racket, однако проблема в том, что это immutable список из racket и мы его как всегда кастим через ```list->mlist``` во что-то приемлемое для нас.

Всё остальное реализуется тривиально, с помощью всех старых добрых процедур вроде assoc, cadr и т.д.

Давайте проверим что оно работает:
```
> (load-file 'natural-schemes)
(natural-schemes ("Громов Михаил Константинович" 42037) 
                 ("Наумов Глеб Германович" 57490) 
                 ("Наумова Дарья Александровна" 34099) 
                 ("Фролов Никита Маркович" 52355) 
                 ("Савина Анна Егоровна" 42515) 
                 ("Юдин Михаил Тимофеевич" 124414) 
                 ("Сорокин Константин Константинович" 41444) 
                 ("Колесов Александр Савельевич" 87464) 
                 ("Горохов Макар Робертович" 52319) 
                 ("Сидоров Тимур Михайлович" 33244))
> (get-record "Громов Михаил Константинович" (load-file 'natural-schemes))
(natural-schemes "Громов Михаил Константинович" 42037)
> (get-salary (get-record "Громов Михаил Константинович" (load-file 'natural-schemes)))
42037
> (get-record "Не Существующий Сотрудник" (load-file 'natural-schemes))
#f
> (find-employee-record "Не Существующий Сотрудник" 
                        (list (load-file 'natural-schemes)))
()
> (find-employee-record "Юдин Михаил Тимофеевич"
                        (list (load-file 'natural-schemes)))
((natural-schemes "Юдин Михаил Тимофеевич" 124414) 
 (natural-schemes "Юдин Михаил Тимофеевич" 124414))
```

### "XR Company" [SQLite]

Давайте сделаем что-то прикольное, возьмём какой-то реальный движок базы данных, которая поддерживает SQL. 
Заполним её сотрудниками и интегрируем в нашу систему незаметно.

Возьмём для этого что-нибудь вроде sqlite3, и исполним следующий код:
```sql
CREATE TABLE IF NOT EXISTS employees (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  salary INTEGER NOT NULL,
  address TEXT,
  phone TEXT
);

INSERT INTO employees (name, salary, address, phone)
VALUES
("Gayle Barnes",      124000, "Dedham, Massachusetts(MA)",      "(314) 725-9276"),
("James Blair",       50040,  "Webster, Massachusetts(MA)",     "(207) 646-7122"),
("Christine Booth",   73414,  "Stoughton, Massachusetts(MA)",   "(781) 436-5875"),
("Susan Bridge",      42414,  "Fall River, Massachusetts(MA)",  "(508) 567-3653"),
("Andrew Bridson",    124124, "Northbridge, Massachusetts(MA)", "(860) 963-0492"),
("Graham Carroll",    42144,  "Franklin, Massachusetts(MA)",    "(817) 276-3240"),
("Belayet Choudhury", 76324,  "Boston, Massachusetts(MA)",      "(617) 541-8336"),
("Phillip Collings",  39486,  "Waltham, Massachusetts(MA)",     "(781) 893-9908"),
("Graham Cooper",     23058,  "Whately, Massachusetts(MA)",     "(615) 228-2158"),
("Glen Dale",         48097,  "Haverhill, Massachusetts(MA)",   "(603) 382-2058"),
("Christian Davies",  58723,  "Roslindale, Massachusetts(MA)",  "(757) 531-9199");
```

Теперь давайте возьмём библиотеку db и напишем соответствующие обвязки:
```racket
(#%require db)

(define (install-xr-company-package)
  (define dbpath
    (build-path *runtime-dir*                
                "assets/xr-company.db"))

  (define (convert lst)
    (map list->mlist (list->mlist lst)))

  (define (load-file)
    (sqlite3-connect #:database dbpath))

  (define (unload-file file)
    (disconnect file))

  (define (get-record employee file)
    (let ((result (query-maybe-row 
                    file 
                    "SELECT id, name, phone, salary, address 
                     FROM employees WHERE name=? LIMIT 1" 
                    employee)))
      (if result
        (vector->list result)
        result)))

  (define get-salary cadddr)

  (put 'load-file 'xr-company load-file)
  (put 'unload-file 'xr-company unload-file)
  (put 'get-record 'xr-company get-record)
  (put 'get-salary 'xr-company get-salary)

  'done)
```

Теперь исполним какие-нибудь такие вещи:
```racket
(define schemes (load-file 'natural-schemes))
(define xr (load-file 'xr-company))

(define companies
  (list schemes xr))

(get-salary (get-record "Сидоров Тимур Михайлович" schemes)) 
(get-salary (get-record "Glen Dale" xr))
(get-salary (get-record "Susan Bridge" xr))
(get-record "Qusan Bridge" xr)
(get-salary (car (find-employee-record "Andrew Bridson" companies)))

(for-each unload-file companies)
```

И получим то, что собственно ожидали:
```
33244
48097
42414
#f
124124
```

Можно даже пойти руками добавить Qusan Bridge
```sql
INSERT INTO employees (name, salary, address, phone) 
VALUES ("Qusan Bridge",  100500, "Dedham, Massachusetts(MA)", "(314) 725-9274");
```

И повторно прогнать код, чтобы увидеть, что теперь уже сотрудник находится:
```
33244
48097
42414
(xr-company 12 "Qusan Bridge" "(314) 725-9274" 100500 "Dedham, Massachusetts(MA)")
124124
```

### Листинг
```racket
#lang sicp

(#%require racket/path)
(#%require racket/runtime-path)
(#%require racket/mpair)
(#%require (only racket
                 void
                 build-path
                 file->list
                 make-hash
                 hash-set!
                 hash-ref))
(#%require db)

(define-runtime-path *runtime-path* "2.74.rkt")
(define *runtime-dir* (path-only *runtime-path*))

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

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error 'type-tag 
             "bad tagged datum ~a" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error 'contents 
             "bad tagged datum ~a" datum)))

(define (get-record employee file)
  (let* ((division (type-tag file))
         (records (contents file))
         (get-record* (get 'get-record division))
         (record (get-record* employee records)))
    (if record 
      (attach-tag division record)
      record)))

(define (get-salary record)
  (let* ((division (type-tag record))
         (data (contents record))
         (get-salary* (get 'get-salary division)))
      (get-salary* data)))

(define (find-employee-record employee files)
  (apply append 
         (map (lambda (file)                             
                (let ((result (get-record employee file)))
                  (if result (list result) '())))
              files)))

(define (load-file division)
  (attach-tag division ((get 'load-file division))))

(define (unload-file file)
  ((get 'unload-file (type-tag file)) (contents file)))

(define (install-natural-schemes-package)
  (define dbpath
    (build-path *runtime-dir*                
                "assets/NaturalSchemes.list"))

  (define (convert lst)
    (map list->mlist (list->mlist lst)))

  (define (load-file)
    (convert (file->list dbpath)))

  (define (unload-file file)
    (void))

  (define get-record assoc)
  (define get-salary cadr)

  (put 'load-file 'natural-schemes load-file)
  (put 'unload-file 'natural-schemes unload-file)
  (put 'get-record 'natural-schemes get-record)
  (put 'get-salary 'natural-schemes get-salary)

  'done)

(define (install-xr-company-package)
  (define dbpath
    (build-path *runtime-dir*                
                "assets/xr-company.db"))

  (define (convert lst)
    (map list->mlist (list->mlist lst)))

  (define (load-file)
    (sqlite3-connect #:database dbpath))

  (define (unload-file file)
    (disconnect file))

  (define (get-record employee file)
    (let ((result (query-maybe-row 
                    file 
                    "SELECT id, name, phone, salary, address 
                     FROM employees WHERE name=? LIMIT 1" 
                    employee)))
      (if result
        (vector->list result)
        result)))

  (define get-salary cadddr)

  (put 'load-file 'xr-company load-file)
  (put 'unload-file 'xr-company unload-file)
  (put 'get-record 'xr-company get-record)
  (put 'get-salary 'xr-company get-salary)

  'done)


(install-natural-schemes-package)
(install-xr-company-package)

(define schemes (load-file 'natural-schemes))
(define xr (load-file 'xr-company))

(define companies
  (list schemes xr))

(get-salary (get-record "Сидоров Тимур Михайлович" schemes)) 
(get-salary (get-record "Glen Dale" xr))
(get-salary (get-record "Susan Bridge" xr))
(get-record "Qusan Bridge" xr)
(get-salary (car (find-employee-record "Andrew Bridson" companies)))

(for-each unload-file companies)
```

## 2.75

Ну это достаточно просто:
```racket
(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imag-part) (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else
           (error 'make-from-mag-ang 
                  "unknown op ~a" op))))
  dispatch)
```

Что мне нравится, так это то как совершенно невзначай по сути мы пришли к тому, что существует более менее в oop. 
На самом деле какое-нибудь прототипное наследование тоже ничто не мешает на примерно подобном базисе соорудить, а при помощи макроса сделать еще и красивым.

Давайте зададим какой-нибудь принтер этой красоты:
```racket
(define (display-complex z)
  (define (round-10 x)
    (/ (round (* (expt 10 10) x))
       (expt 10 10)))
  (for-each (lambda (x) 
              (display x) 
              (newline))
            (list (list 'magnitude (round-10 (magnitude z)))
                  (list 'angle (round-10 (angle z)))
                  (list 'real (round-10 (real-part z)))
                  (list 'imag (round-10 (imag-part z))))))
```

И можно увидеть что подход действительно работает, совершенно разные комплексные числа прекрасно перемножаются:
```
> (display-complex
    (mul-complex (make-from-real-imag (/ 1 (sqrt 2)) (/ 1 (sqrt 2)))
                 (make-from-mag-ang 3 (/ pi 4)))) 
(magnitude 3.0)
(angle 1.5707963268)
(real 0.0)
(imag 3.0)
```

### Листинг

```racket
#lang sicp

(#%require (only racket 
                 pi))
(define (square x) (* x x))

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imag-part) (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else
           (error 'make-from-mag-ang 
                  "unknown op ~a" op))))
  dispatch)

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error 'make-from-real-imag 
                  "unknown op ~a" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))

(define (add-complex z1 z2)
  (make-from-real-imag 
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag 
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang 
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang 
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

(define (display-complex z)
  (define (round-10 x)
    (/ (round (* (expt 10 10) x))
       (expt 10 10)))
  (for-each (lambda (x) 
              (display x) 
              (newline))
            (list (list 'magnitude (round-10 (magnitude z)))
                  (list 'angle (round-10 (angle z)))
                  (list 'real (round-10 (real-part z)))
                  (list 'imag (round-10 (imag-part z))))))
```

## 2.76

Мы будем следить не за количеством кода, которое необходимо добавить, а за количеством старого кода который надо изменить, что привнести новое поведение или тип. Соответственно чем меньше подобных изменений, тем лучше.

### Explicit dispatch

Итак, каждая функция знает с какими типами она работает.

Если мы добавляем новую функцию, то толком ничего не меняется, что замечательно.

Из плюсов: поведение для каждого типа локализовано в операции, поэтому нам не надо прыгать туда-сюда между файлами, чтобы добавить новое поведение ряду объектов.
Но из минусов, каждый тип требует включения, поэтому мы не можем распараллелить работу, что плохо.

Если мы добавляем новый тип, то всё печально: придётся руками пофиксить все операции, чтобы они включали и новый тип тоже.

### Message passing

Когда мы добавляем новое поведение, то каждый объект, который мы включаем в систему необходимо изменить, чтобы добавить ему соответствующий метод. Короче дорого: меняется много существующего кода. Но из плюсов: работа спокойно параллелится.

Добавление нового типа же весьма дешевое: мы просто пишем новое определение объекта и он безболезненно интегрируется в существующую систему.

### Data-driven 

Когда мы добавляем новое поведение, то нам достаточно написать новый код и зарегестрировать его внутри пакета, которого это касается. То есть ноль изменений, только новый код. 

При этом работа параллелится, но это зависит от того, как пакеты поделены.

Добавление нового типа в систему тоже весьма дешевое: надо написать весь код для пакета в изоляции и зарегистрировать его в системе. Никаких дополнительных изменений делать не надо.

### Много новых типов

Однозначно не выигрывает explicit dispatch, можно рассматривать примерно с одинаковой эффективностью message passing и data-driven дизайн, на самом деле в пользу message passing. Почему? Потому что message passing работает не на уровне пакетов, а на уровне самих объектов. 

То есть чтобы зарегистрировать новый тип, достаточно предоставить конструктор объекта, который является всего лишь функцией. Это позволяет создавать "временные типы", которые участвуют только в определенной стадии жизни программы. data-driven дизайн такой красотой не обладает и является намного более сложной штукой: нам надо написать пакет, загрузить его, выгрузить его.

### Много новых функций

Тут всё сложно. Когда у нас небольшая коллекция типов, однозначным победителем становится direct dispatch.
Ну условно у нас бывает 4 варианта типов данных и сотни функций над ними оперирующих. Замечательно.

Если коллекция типов большая, то direct dispatch начинает быть проблематичным. И скорее должен быть заменен либо на data-driven, либо на message passing. Скорее в пользу data-driven, потому что изменения не требуются в конструкторах, а только в поведении, что уменьшает поверхность возникновения всяких багов.
