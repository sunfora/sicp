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
```
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
   "2.4.3 Data-Directed Programming and
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
