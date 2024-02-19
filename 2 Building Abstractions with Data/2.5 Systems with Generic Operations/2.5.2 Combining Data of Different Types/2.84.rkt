#lang sicp
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

(define generics (setup-table 'generics))
(define coercion (setup-table 'coercion))
(define tower (setup-type-tower))


;; type data primitives
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
  
  (define (apply-loop args)
    (cond ((has? args)
           (apply-strip args))
          ((can-be-raised? args)
           (apply-loop (raise-all args)))
          (else 
            (method-not-found))))
  (apply-loop args))

;; packages 

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))

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
  (generics 'put 'equ? '(integer integer) =)
  (generics 'put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (generics 'put 'make 'integer make)
  'done)
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))

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
  (define (eq-rat? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))

  (define (=zero? x)
    (= (numer x) 0))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (generics 'put 'numer '(rational) numer)
  (generics 'put 'denom '(rational) denom)
  (generics 'put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (generics 'put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (generics 'put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (generics 'put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (generics 'put 'equ? '(rational rational) eq-rat?)
  (generics 'put '=zero? '(rational) =zero?)
  (generics 'put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
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
(define (install-rectangular-package)
  ;; internal procedures
  (define (square x) (* x x))
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
  (define (square x) (* x x))
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
  (define (eq-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (=zero? z1)
    (= (magnitude z1) 0))

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
  (generics 'put '=zero? '(complex) =zero?)
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


;; generic operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (make-integer n)
  ((generics 'get 'make 'integer) n))
(define (make-rational n d)
  ((generics 'get 'make 'rational) n d))
(define (make-real n)
  ((generics 'get 'make 'real) n))

(define (numer rat)
  (apply-generic 'numer rat))
(define (denom rat)
  (apply-generic 'denom rat))

(define (make-complex-from-real-imag x y)
  ((generics 'get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((generics 'get 'make-from-mag-ang 'complex) r a))
(define make-complex make-complex-from-real-imag)

(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))

;; type tower
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

;; installed packages
(install-integer-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-numerical-tower-package)
