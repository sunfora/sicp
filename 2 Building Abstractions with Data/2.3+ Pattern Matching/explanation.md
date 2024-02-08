# 2.3.5 Pattern Matching and Rule-based Substitution

Этот контент не из книги, не ищите его в ней, вы его не найдёте.

Это контент вот этой лекции, которая тоже часть курса:
[![](https://img.youtube.com/vi/_fXQ1SwKjDg/0.jpg)](https://www.youtube.com/watch?v=_fXQ1SwKjDg)

В общем чем мы тут займёмся? Сначала сходите посмотрите лекцию.
И мы немного поправим код на доске, чтобы он работал с современным scheme, короче сами напишем всё.

Для начала по примеру лекции просто заведем парочку правил, которые мы затем расширим:

```racket
#lang sicp 

(define deriv-rules
  '(
    ((dd (?c c) (? v)) 0)
    ((dd (?v v) (? v)) 1)
    ((dd (?v u) (? u)) 0)

    ((dd (+ (? a) (? b)) (? v)) 
     (+ (dd (: a) (: v))
        (dd (: b) (: v))))

    ((dd (- (? a) (? b)) (? v))
     (- (dd (: a) (: v))
        (dd (: b) (: v))))

    ((dd (* (? a) (? b)) (? v))
     (+ (* (dd (: a) (: v))
           (: b))
        (* (: a)
           (dd (: b) (: v)))))))
```

## Matcher

Теперь перейдём к matcher-у, и я сразу сделаю несколько изменений по сравнению с тем, что нам предлагают написать:
1. Во первых давайте откажемся от идеи руками писать ```'failed```, потому что это плохая идея:
   
   Заведем мы для всего соответствующие предикаты и конструкторы:
   ```racket
   (define (fail dict)
     false)

   (define (failed? dict) 
     (not dict))
   ```
2. Вынесем все match-и в отдельные функции.

Получим что-то вроде такого:

```racket
(define (arbitrary-constant? pat)
  (eq? '?c (car pat)))

(define (arbitrary-variable? pat)
  (eq? '?v (car pat)))

(define (arbitrary-expression? pat)
  (eq? '? (car pat)))

(define (variable-name pat)
  (cadr pat))

(define (atom? x)
  (not (pair? x)))

(define (match pat exp dict)
  (cond ((failed? dict) dict)
        ((atom? pat)
         (match-atomic pat exp dict))
        ((arbitrary-constant? pat)
         (match-constant pat exp dict))
        ((arbitrary-variable? pat)
         (match-variable pat exp dict))
        ((arbitrary-expression? pat)
         (match-expression pat exp dict))
        ((atom? exp)
         (fail dict))
        (else 
         (match (car pat)
                (car exp)
                (match (cdr pat)
                       (cdr exp)
                       dict)))))
```

И теперь можно по-отдельности дописать всё что нам нужно, уже более не меняя match.

```racket
(define (match-atomic pat exp dict)
  (if (eq? pat exp)
    dict
    (fail dict)))
```

Сразу проверим как оно работает:
```
> (match-atomic 'foo 'foo '())
()
> (match-atomic 'foo 'fox '())
#f
> (match-atomic '() '() '())
()
```

Ну замечательно работает, единственное что теперь бы хотелось как-то понять как сделать ```dict```.

dict в нашем случае это всего лишь набор: переменные значения.

Мы уже в прошлом разделе что-то такое делали, в частности мы там всякую дичь творили вроде AVL деревьев.
Просто множеств сотканных из списков. В принципе для строк лучше подойтёт какое-нибудь trie или hash-map лучше.

Но это всё у нас из коробки не доступно, зато доступны так называемые association lists, а точнее предикаты вродеде ```assq```, ```assv```, ```assoc```. Они по сути то же самое что ```eq?```, ```eqv?```, ```equal?```.

Нам надо как-то добавить в словарь новую переменную, либо проверить, что она уже существует и её значение совпадает с тем, что мы хотим, поэтому мы напишем следующий метод для словаря:
```racket
(define (extend-dict pat value dict) 
  (let* ((name (variable-name pat))
         (p (assq name dict)))
    (cond ((not p) (cons (list name value) dict))
          ((equal? (cadr p) value) dict)
          (else (fail dict)))))
```

И теперь можно приступить к всем нашим случаям: 
```racket
(define (match-pred pred?)
  (lambda (pat exp dict)
    (if (pred? exp)
      (extend-dict pat exp dict)
      (fail dict))))

(define constant? number?)
(define variable? symbol?)
(define expression? (lambda (x) true))

(define match-constant (match-pred constant?))
(define match-variable (match-pred variable?))
(define match-expression (match-pred expression?))
```

Давайте теперь опробуем и убедимся что оно работает:
```
> (match '(dd (- (? a) (? b)) (? v)) '(dd (- a b) v) '())
((a a) (b b) (v v))
> (match '(dd (- (? a) (? b)) (? v)) '(dd (- a b) a) '())
((a a) (b b) (v a))
> (match '(dd (- (? a) (? b)) (? a)) '(dd (- a b) a) '())
((b b) (a a))
> (match '(dd (- (? a) (? b)) (? a)) '(dd (- a b) v) '())
#f
> (match '(dd (- (?c a) (? b)) (? v)) '(dd (- x y) z) '())
#f
> (match '(dd (- (?c a) (? b)) (? v)) '(dd (- 1/2 y) z) '())
((a 1/2) (b y) (v z))
> (match '(dd (- (?c a) (?v b)) (? v)) '(dd (- 1/2 +) y) '())
((a 1/2) (b +) (v y))
```

Последний кейс мне не нравится, ну потому что не хочется чтобы + внезапно стал переменной.
Поэтому давайте поправим то что мы наспех сделали:

Нам надо как-то сделать так, чтобы переменной считалось что-то, что содержит в себе символы и возможно разделения вроде ```-``` и ```_```.

## Парсим variable

Для этого нам придётся строчку попарсить примерно следующей грамматикой:
```
variable -> word delim word

word -> alpha-char
word -> alpha-char word

delim -> delim-char
delim -> delim-char delim

delim-char -> -
delim-char -> _
```

Давайте это как-нибудь по-простому сделаем:
```racket
(define (delimiter? char)
  (if (member char '(#\- #\_)) true false))

(define (parse-failed? result)
  (not (cadr result)))

(define (parse-fail result)
  (list (car result)
        false))

(define (make-result parsed rest)
  (list parsed rest))

(define (parse-parsed result)
  (car result))

(define (parse-rest result)
  (cadr result))
```

Заведём вот такие вот предикатики, конструкторы и селекторы, чтобы в дальнейшнем нам было проще парсить.

И теперь давайте сделаем основной парсер, который будет делать за нас работу, а именно: ```take-pred```.
Он будет от списка символов откусывать ноль или больше вхождений некого ```pred?```.

```racket
(define (take-pred pred? chars)
  (cond ((and (pair? chars)
              (pred? (car chars)))
         (let* ((result (take-pred pred? (cdr chars)))
                (parsed (parse-parsed result))
                (rest (parse-rest result)))
           (make-result (inc parsed) rest)))
        (else (make-result 0 chars))))
```

Ну держите сразу пример работы:
```
> (take-pred char-numeric? '(#\1 #\2 #\3 #\a #\b))
(3 (#\a #\b))
```

Теперь дело техники из этой процедуры сделать процедурки ```take-word```, ```take-delim```.
Единственное их отличие, что они должный фейлится если они попарсили ноль символов, для этого мы их обернем просто вокруг специальной функции, да и всё.

```racket
(define (zero-parsed?=>parse-fail parser)
  (lambda (chars)
    (let* ((result (parser chars))
           (parsed (parse-parsed result)))
      (if (or (parse-failed? result)
              (zero? parsed))
        (parse-fail result)
        result))))

(define take-word
  (zero-parsed?=>parse-fail 
    (lambda (chars)
      (take-pred char-alphabetic? chars))))

(define take-delim
  (zero-parsed?=>parse-fail 
    (lambda (chars)
      (take-pred delimiter? chars))))
```

Ну и наконец гвоздь нашей программы, variable.
```racket
(define (take-variable chars)
  (let* ((word-result (take-word chars))
         (word-rest   (parse-rest word-result))
         (delim-result  (take-delim word-rest))
         (delim-rest (parse-rest delim-result)))
    (if (not (parse-failed? delim-result))
      (take-variable delim-rest)
      word-result)))
```

Мы пытаемся попарсить сразу слово и делимитер, и если второй попарсился, то снова парсим.
Иначе результат зависит от того, попарсился ли первый.

Теперь давайте сделаем предикат ```parsed-string?```, который будет давать зеленый свет в случае если парсер дошел до конца строки и красный если иначе.
```racket
(define (parsed-string? parser str)
  (let* ((chars (string->list str))
         (result (parser chars))
         (rest (parse-rest result)))
    (null? rest)))

(define (parsed-symbol? parser symbol)
  (parsed-string? parser (symbol->string symbol)))
```

Осталось обернуть это всё в предикат ```variable?```:
```racket
(define (variable? expr)
  (and (symbol? expr)
       (parsed-symbol? take-variable expr)))
```

И давайте проверим на каких-нибудь примерах:
```
> (variable? 'fewfwe-fewwef)
#t
> (variable? 'fewfwe-)
#f
> (variable? 'fewfwe)
#t
> (variable? '?fwe
#f
> (variable? 'ewef_efwfw)
#t
```

Теперь если мы возвратимся к старому примеру и попробуем вызвать match, то получим уже фейл.
```
> (match '(dd (- (?c a) (?v b)) (? v)) '(dd (- 1/2 +) y) '())
#f
```

## Instantiate

Ну отлично, давайте теперь сделаем ```instantiate```.

Для этого заведем ```walk``` и ```tree-map```
```racket
(define (walk f tree)
  (define (walk-f tree) (walk f tree))
  (cond ((null? tree) tree)
        ((list? tree) (f (map walk-f tree)))
        (else (f tree))))

(define (tree-map f tree)
  (walk (lambda (tree)
          (if (list? tree)
            tree
            (f tree)))
        tree))

```

Сам метод выглядит примерно следующим образом:
```racket
(define (instantiate skeleton dict)
  (walk (lambda (s)
          (cond ((skeleton-evaluation? s) 
                 (evaluate (skeleton-expr s)))
                ((skeleton-substitution? s)
                 (substitute (skeleton-expr s) 
                             dict))
                (else s)))
        skeleton))

(define (skeleton-evaluation? skeleton)
  (and (pair? skeleton)
       (eq? ':e (car skeleton))))

(define (skeleton-substitution? skeleton)
  (and (pair? skeleton)
       (eq? ': (car skeleton))))

(define (skeleton-expr skeleton)
  (if (not (null? (cddr skeleton)))
    (error "expected one argument for skeleton" skeleton))
  (cadr skeleton))

(define (lookup var dict)
  (if (failed? dict)
    (error "trying to evaluate failed dict"))
  (let ((value (assq var dict)))
    (if value
      (cadr value)
      var)))

(define (evaluate expr)
  (eval expr
        (interaction-environment)))

(define (substitute expr dict)
  (define (look var)
    (lookup var dict))
  (car (tree-map look (list expr))))
```

Короче я немного расширил язык. Раньше как было? Раньше когда ты печатал (: (op x y)), подставлялись соответственно ```op```, ```x```, ```y```, и получалось например что-нибудь вроде: ```(+ 1 2)```, после чего в текущем окружении находился ```+``` и делалалось ```(apply + (1 2))```, что приводило к тому, что эта билеберда вычислялась. 

Нафига такое свойство? Ну чтобы короче вычислять константы по идее.

Но мне не нравится ни реализация ни идея. Ну типа наверное это всё же должна быть отдельная операция, давайте её так и назовём: ```(:e <expr>)```. И я еще настаиваю на том, что эти две вещи друг друга можно вкладывать:

```
> (instantiate '(:e ((lambda (x y) (+ (: y) y)) (: x) (: y))) '((x 5) (y 6)))
12
```

Что за шизофрения происходит? Ну смотрите сначала вычисляются подстановки внутри ```(:e )```, ну и вообще в общем случае сначала вычисляется то, что внутри, и только потом вычисляется наружнее. 

Происходит следующая серия подстановок:
```
(:e ((lambda (x y) (+ (: y) y)) (: x) (: y)))
(:e ((lambda (x y) (+ 6 y)) 5 6))
(:e ((lambda (x y) (+ 6 y)) 5 6))
```

Последнее вычисляется по всем обыкновенным правилам лиспа и получается в итоге 12.

Короче ```(:e )``` позволяет мне засунуть произвольный лисповый код внутрь наших подстановок, в том числе например вот такую красоту, которая подставляет из двух минимальное значение.
```
(: (:e (if (< (: x) (: y)) 'x 'y)))
```

Может быть еще не хватает какого-нибудь splice оператора, который бы убирал скобки у списка. На вроде вот такого:
```
(: (:@ (:e (list 'x 'y))))
(: (:@ (x y)))
(: x y)
```

Но это потребует переписывания instantiate.

```racket
(define (instantiate skeleton dict)
  (define (transform-nosplice s)
    (cond ((skeleton-evaluation? s) 
           (evaluate (skeleton-expr s)))
          ((skeleton-substitution? s)
           (substitute (skeleton-expr s) dict))
          (else s)))  

  (define (listify result)
    (if (skeleton-splice? result)
      (skeleton-expr result)
      (list result)))

  (define (delistify s)
    (if (atom? s)
      s
      (apply append s)))

  (define (transform s)
    (listify 
      (transform-nosplice 
        (delistify s))))

  (let ((result (walk transform skeleton)))
    (if (null? (cdr result))
      (car result)
      result)))
```

Что происходит? Ну примерно то же, что и раньше. 
Но теперь мы перед каждой трансформацией мержим списки. Выполняем трансформацию. И результат повышаем до списка.

Ну короче хочется чего-то такого, вот на каком-то этапе мы получили список вида:
```
(1 2 (:@ 3 4))
```

Нам хочется чтобы он был не таким, а вот таким:
```
((1) (2) (3 4))
```
Тогда сделав append, перед очередной трансформацией, мы получим эффект равный сплайсу.
```
(1 2 3 4)
```

Проще на самом деле какой-то пример разобрать, звёздочкой я буду показывать где мы на данный момент находимся:
```
x = 5, y = 6

*((: x) (: y) (:@ (1 2)))
(*(: x) (: y) (:@ (1 2)))
((*: x) (: y) (:@ (1 2)))
(((:) *x) (: y) (:@ (1 2)))
(((:) (x)) (: y) (:@ (1 2)))
(*((:) (x)) (: y) (:@ (1 2)))
(*(: x) (: y) (:@ (1 2)))
(*5 (: y) (:@ (1 2)))
(*(5) (: y) (:@ (1 2)))
((5) *(: y) (:@ (1 2)))
((5) (*: y) (:@ (1 2)))
((5) ((:) *y) (:@ (1 2)))
((5) ((:) *(y)) (:@ (1 2)))
((5) *((:) (y)) (:@ (1 2)))
((5) *(: y) (:@ (1 2)))
((5) *6 (:@ (1 2)))
((5) *(6) (:@ (1 2)))
((5) (6) *(:@ (1 2)))
((5) (6) (*:@ (1 2)))
((5) (6) ((:@) *(1 2)))
((5) (6) ((:@) (*1 2)))
((5) (6) ((:@) ((1) *2)))
((5) (6) ((:@) *((1) (2))))
((5) (6) ((:@) *(1 2)))
((5) (6) ((:@) *((1 2))))
((5) (6) *((:@) ((1 2))))
((5) (6) *(:@ (1 2)))
((5) (6) *(1 2))
*((5) (6) (1 2))
*(5 6 1 2)
((5 6 1 2))
```

Ну вы поняли я надеюсь, на самом деле ничего сложного, просто оно бегает туда сюда и делает бесполезную работу, а потом полезную иногда.

Зато теперь можно творить всякую дичь, например конструировать функции:
```racket
(define or-set 
  (instantiate '(:e (lambda (x)
                      (or (:@ (:e (map (lambda (n) (list 'equal? n 'x))
                                     (list (:@ (: x)))))))))
             '((x ("what" "the" "hell" 1)))))
```

Сперва подставляется x, как список ```(a b ...)```, потом все элементы попадают в конструктор списка ```(list a b ...)```. Затем мы делаем новый список вида ```((equal? a x) (equal? b x) ... )```. Засовываем всё это в ```or```. И получаем на выходе лямбду, которая по сути является множеством что содержит все эти элементы?

В общем, та еще дичь.

Теперь мы можем это всё потыкать:
```
> (or-set "what")
#t
> (or-set "the")
#t
> (or-set "hell")
#t
> (or-set 1)
#t
> (or-set 2)
#f
```

Ну на этом мы покончим с instantiation и передвинемся к чему-то более содержательному.
Но перед этим я хочу заметить, что всякие макросы вроде того, что в лиспе есть сделаны примерно так же по сути.


