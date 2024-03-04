#lang sicp
(#%require (only racket
                 make-hash
                 hash-set!
                 hash-ref))
(define (setup-table . name-list)
  (define name
    (cond 
      ((= 0 (length name-list))
       'anonymous)
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

;; ======================================================
;; number package
;; ======================================================
(define (install-number-package export-generics export-tag)
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
  (let ((generics (setup-table 'numer-generics))
        (coercion (setup-table 'coercion))
        (tower (setup-type-tower)))
    ;; type data primitives
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

    ;; generic
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
          (if (equ? (raise result) obj)
            (drop result) 
            obj))
        obj))

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
      
      (define (drop-if-can arg)
        (if (and (pair? arg)
                 (tower 'in (type-tag arg)))
          (drop arg)
          arg))

      (define (raise-loop args)
        (cond ((has? args)
               (apply-strip args))
              ((can-be-raised? args)
               (raise-loop (raise-all args)))
              (else
                (method-not-found))))

      (define (apply-loop args)
        (if (has? args)
          (apply-strip args)
          (let ((dropped (map drop-if-can args)))
            (raise-loop dropped))))
      (drop-if-can (apply-loop args)))

    ;; packages 

    (define (install-integer-package generics)
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
      (generics 'put 'negate '(integer)
           (lambda (x) (tag (- x))))
      (generics 'put 'repr '(integer) number->string)
      (generics 'put 'make 'integer make)
      'done)

    (define (install-rational-package generics)
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

      
      (define (negate x)
        (make-rat (- (numer x))
                  (denom x)))


      (define (repr r)
        (string-append 
          (number->string (numer r))
          "/"
          (number->string (denom r))))
      
      (define (gcd-rat x y)
        (let ((result (gcd (/ (numer x) (denom x))
                           (/ (numer y) (denom y)))))
          (make-rat (numerator result)
                    (denominator result))))

      ;; interface to rest of the system
      (define (tag x) (attach-tag 'rational x))
      (generics 'put 'numer '(rational) 
                (lambda (x) 
                  (make-integer (numer x))))
      (generics 'put 'denom '(rational) 
                (lambda (x) 
                  (make-integer (denom x))))
      (generics 'put 'add '(rational rational)
           (lambda (x y) (tag (add-rat x y))))
      (generics 'put 'sub '(rational rational)
           (lambda (x y) (tag (sub-rat x y))))
      (generics 'put 'mul '(rational rational)
           (lambda (x y) (tag (mul-rat x y))))
      (generics 'put 'div '(rational rational)
           (lambda (x y) (tag (div-rat x y))))
      (generics 'put 'negate '(rational) 
           (lambda (x) (tag (negate x))))
      (generics 'put 'greatest-common-divisor '(rational rational)
           (lambda (x y)
             (tag (gcd-rat x y))))
      (generics 'put 'equ? '(rational rational) eq-rat?)
      (generics 'put '=zero? '(rational) =zero?)
      (generics 'put 'repr '(rational) repr)
      (generics 'put 'make 'rational
           (lambda (n d)
             (if (or (not (eq? (type-tag n) 'integer))
                     (not (eq? (type-tag d) 'integer)))
                 (error 'rational-package/make
                        "cannot create rational not from integers")
                 (let ((n (integer->scheme-number n))
                       (d (integer->scheme-number d)))
                   (tag (make-rat n d))))))
      'done)

    (define (install-real-package generics)
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
      (generics 'put 'negate '(real)
           (lambda (x) (tag (- x))))
      (generics 'put 'sine '(real) 
           (lambda (x) (tag (sin x))))
      (generics 'put 'cosine '(real)
           (lambda (x) (tag (cos x))))
      (generics 'put 'arctan '(real real) 
           (lambda (x y) (tag (atan x y))))
      (generics 'put 'square-root '(real)
           (lambda (x) (tag (sqrt x))))

      (generics 'put 'equ? '(real real) =)
      (generics 'put 'repr '(real) number->string)
      (generics 'put '=zero? '(real)
           (lambda (x) (= x 0)))
      (generics 'put 'make 'real make)
      'done)

    (define (install-rectangular-package generics)
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

      (define (negate-rectangular z)
        (make-from-real-imag (negate (real-part z))
                             (negate (imag-part z))))
      ;; interface to the rest of the system
      (define (tag x) 
        (attach-tag 'rectangular x))
      (generics 'put 'real-part '(rectangular) real-part)
      (generics 'put 'imag-part '(rectangular) imag-part)
      (generics 'put 'magnitude '(rectangular) magnitude)
      (generics 'put 'angle '(rectangular) angle)
      (generics 'put 'negate '(rectangular)
           (lambda (x) (tag (negate-rectangular x))))
      (generics 'put 'make-from-real-imag 'rectangular
           (lambda (x y) 
             (tag (make-from-real-imag x y))))
      (generics 'put 'make-from-mag-ang 'rectangular
           (lambda (r a) 
             (tag (make-from-mag-ang r a))))
      'done)

    (define (install-polar-package generics)
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

      (define (negate-polar x)
        (make-from-mag-ang
          (negate (magnitude x))
          (angle x)))
      ;; interface to the rest of the system
      (define (tag x) (attach-tag 'polar x))
      (generics 'put 'real-part '(polar) real-part)
      (generics 'put 'imag-part '(polar) imag-part)
      (generics 'put 'magnitude '(polar) magnitude)
      (generics 'put 'angle '(polar) angle)
      (generics 'put 'negate '(polar)
           (lambda (x) (tag (negate-polar x))))
      (generics 'put 'make-from-real-imag 'polar
           (lambda (x y) 
             (tag (make-from-real-imag x y))))
      (generics 'put 'make-from-mag-ang 'polar
           (lambda (r a) 
             (tag (make-from-mag-ang r a))))
      'done)

    (define (install-complex-package generics)
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

      (define (complex-=zero? z1)
        (=zero? (magnitude z1)))

      (define (repr-complex c)
        (string-append 
          (repr (real-part c))
          "+"
          (repr (imag-part c))
          "i"))

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
      (generics 'put '=zero? '(complex) complex-=zero?)
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
      (generics 'put 'negate '(complex) 
           (lambda (z) (tag (negate z))))
      (generics 'put 'repr '(complex) repr-complex)
      'done)

    ;; miscellaneous
    (define (integer->scheme-number i)
      (contents i))

    ;; generic operations
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
    (define (greatest-common-divisor x y) (apply-generic 'greatest-common-divisor x y))

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

    (define (real-part z) 
      (apply-generic 'real-part z))
    (define (imag-part z) 
      (apply-generic 'imag-part z))
    (define (magnitude z) 
      (apply-generic 'magnitude z))
    (define (angle z) 
      (apply-generic 'angle z))
    ;; type tower
    (define (install-numerical-tower-package generics coercion tower)
      (tower 'register 'integer)
      (tower 'register 'rational)
      (tower 'register 'real)
      (tower 'register 'complex)

      (define (make-integer a)
        ((generics 'get 'make 'integer) a))
      (define (make-rational a b)
        ((generics 'get 'make 'rational) a b))
      (define (make-complex a b)
        ((generics 'get 'make-from-real-imag 'complex) a b))
      (define (make-real x)
        ((generics 'get 'make 'real) x))

      (define (integer->rational i)
        (make-rational i
                       (make-integer 1)))
      (define (rational->real r)
        (make-real (/ (integer->scheme-number (numer r)) 
                      (integer->scheme-number (denom r)))))
      (define (real->complex r)
        (make-complex (drop r) (make-integer 0)))

      (define (integer->real x)
        (rational->real (integer->rational x)))

      (coercion 'put 'integer 'rational integer->rational)
      (coercion 'put 'rational 'real rational->real)
      (coercion 'put 'real 'complex real->complex)
      (coercion 'put 'integer 'real integer->real)
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

      (define (complex->real c)
        (let ((r (real-part c)))
          (cond ((eq? 'real (type-tag r))
                 r)
                ((coercion 'get (type-tag r) 'real)
                 ((coercion 'get (type-tag r) 'real) r))
                (else
                 (error 'complex->real
                        "cannot coerce real-part ~a of complex ~a"
                        r c)))))

      (coercion 'put 'rational 'integer  rational->integer)
      (coercion 'put 'real 'rational  real->rational)
      (coercion 'put 'complex 'real  complex->real)
      'done)

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
             (export-tag 'number (numer x))))
    (export-generics 'put 'denom '(number)
           (lambda (x)
             (export-tag 'number (denom x))))
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
    (export-generics 'put 'greatest-common-divisor '(number number)
           (lambda (x y)
             (export-tag 'number (greatest-common-divisor x y))))
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
'done)

;; ======================================================
;; scheme-number package
;; ======================================================
(define (install-scheme-number-package generics export-tag)
    (define (tag x)
      (export-tag 'scheme-number x))
    (define (make x)
      (if (not (number? x))
        (error 'scheme-number-package/make
               "expected number but got ~a"
               x))
        (tag x))

    (define real-part 
      (eval 'real-part (scheme-report-environment 5)))
    (define imag-part 
      (eval 'imag-part (scheme-report-environment 5))) 

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
    (generics 'put 'real-part '(scheme-number)
         (lambda (x) (tag (real-part x))))
    (generics 'put 'imag-part '(scheme-number)
         (lambda (x) (tag (imag-part x))))
    (generics 'put 'numer '(scheme-number)
         (lambda (x) (tag (numerator x))))
    (generics 'put 'denom '(scheme-number)
         (lambda (x) (tag (denominator x))))
    (generics 'put 'greatest-common-divisor '(scheme-number scheme-number)
         (lambda (x y) (tag (gcd x y))))
    (generics 'put 'equ? '(scheme-number scheme-number) =)
    (generics 'put 'repr '(scheme-number) number->string)
    (generics 'put '=zero? '(scheme-number)
         (lambda (x) (= x 0)))

    (generics 'put 'make 'scheme-number make)
    'done)

;; ======================================================
;; term package
;; ======================================================
(define (install-term-package generics attach-tag)
  (define (make order coeff)
    (list order coeff))
  (define (order term)
    (car term))
  (define (coeff term)
    (cadr term))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'term x))
  (generics 'put 'order '(term) order)
  (generics 'put 'coeff '(term) coeff)
  (generics 'put 'make 'term
            (lambda (x y)
              (tag (make x y))))
  'done)

;; ======================================================
;; dense package
;; ======================================================
(define (install-dense-package generics attach-tag)
  (define (vals L)
    (if (empty-termlist? L)
      '() (cdr L)))
  (define (the-empty-termlist) '())

  (define (empty-termlist? L)
    (null? L))
  (define (first-term term-list) 
    (make-term (car term-list)
               (cadr term-list)))
  (define (rest-terms term-list)
    (if (empty-termlist? (cddr term-list))
      (the-empty-termlist)
      (cons (inc (car term-list))
            (cddr term-list))))

  (define (term->termlist term)
    (make (order term) (list (coeff term))))

  (define (join L1 L2)
    (define (prepend-zero L)
      (cons (dec (car L))
            (cons 0 (cdr L))))
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          ((order> (first-term L1) (first-term L2))
           (join L2 L1))
          ((order< (first-term L1) (first-term L2))
           (join L1 (prepend-zero L2)))
          ((order= (first-term L1)
                   (first-term L2))
           (let* ((t1 (first-term L1))
                  (t2 (first-term L2))
                  (x (coeff t1))
                  (y (coeff t2))
                  (result (vals (join (rest-terms L1)
                                      (rest-terms L2)))))
             (cond ((and (not (=zero? x))
                         (not (=zero? y)))
                    (error 'join
                           "can't join ~a ~a"
                           L1 L2))
                   ((=zero? x)
                    (append (term->termlist t2) result))
                   ((=zero? y)
                    (append (term->termlist t1) result)))))))

  (define (make order vals)
    (if (null? vals)
      vals
      (cons order vals)))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'dense x))
  (generics 'put 'empty-termlist? '(dense) empty-termlist?)
  (generics 'put 'first-term '(dense) first-term)
  (generics 'put 'rest-terms '(dense)
            (lambda (x)
              (tag (rest-terms x))))
  (generics 'put 'join '(dense dense)
            (lambda (x y)
              (tag (join x y))))
  (generics 'put 'make 'dense 
            (lambda (x y)
              (tag (make x y))))
  'done)

;; ======================================================
;; sparse package
;; ======================================================
(define (install-sparse-package generics attach-tag)
  (define (join L1 L2)
    (cond ((empty-termlist? L1)
           L2)
          ((empty-termlist? L2)
           L1)
          ((order> (first-term L1) (first-term L2))
           (join L2 L1))
          ((order< (first-term L1) (first-term L2))
           (if (=zero? (coeff (first-term L1)))
             (join (rest-terms L1) L2)
             (cons (first-term L1)
                   (join (rest-terms L1) L2))))
          (else
           (let* ((t1 (first-term L1))
                  (c1 (coeff t1))
                  (t2 (first-term L2))
                  (c2 (coeff t2))
                  (result (join (rest-terms L1)
                                (rest-terms L2))))
             (cond ((and (=zero? c1)
                         (=zero? c2))
                    result)
                   ((=zero? c1)
                    (cons t2 result))
                   ((=zero? c2)
                    (cons t1 result))
                   (else
                     (error 'join
                            "can't join ~a ~a"
                            L1 L2)))))))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) 
    (null? term-list))

  (define (foldr start op seq)
      (if (null? seq)
        start 
        (foldr (op start (car seq)) op (cdr seq))))
  (define (make lst)
    (foldr (the-empty-termlist)
           join 
           (map (lambda (x) 
                  (let ((t (make-term (car x) (cadr x))))
                    (if (=zero? (coeff t))
                      (the-empty-termlist)
                      (list t))))
                lst)))
  ;; interface
  (define (tag x)
    (attach-tag 'sparse x))
  (generics 'put 'make 'sparse
            (lambda (x)
              (tag (make x))))
  (generics 'put 'join '(sparse sparse)
            (lambda (x y)
              (tag (join x y))))
  (generics 'put 'first-term '(sparse) first-term)
  (generics 'put 'rest-terms '(sparse)
            (lambda (x)
              (tag (rest-terms x))))
  (generics 'put 'empty-termlist? '(sparse) empty-termlist?)
  'done)

;; ======================================================
;; rational package 
;; ======================================================
(define (install-rational-package generics attach-tag)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (make-rat n d)
    (cons n d))

  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (eq-rat? x y)
    (equ? (mul (numer x) (denom y))
          (mul (numer y) (denom x))))

  (define (zero-rat? x)
    (=zero? (numer x)))

  
  (define (negate-rat x)
    (make-rat (negate (numer x))
              (denom x)))


  (define (repr-rat r)
    (string-append 
      (repr (numer r))
      "/"
      (repr (denom r))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (generics 'put 'numer '(rational) numer) 
  (generics 'put 'denom '(rational) denom) 
            (lambda (x) 
              (make-integer (denom x))))
  (generics 'put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (generics 'put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (generics 'put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (generics 'put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (generics 'put 'negate '(rational) 
       (lambda (x) (tag (negate-rat x))))
  (generics 'put 'equ? '(rational rational) eq-rat?)
  (generics 'put '=zero? '(rational) zero-rat?)
  (generics 'put 'repr '(rational) repr-rat)
  (generics 'put 'make 'rational
       (lambda (n d)
         (tag (make-rat n d))))
  'done)

;; ======================================================
;; polynomial package
;; ======================================================
(define (install-polynomial-package generics attach-tag)
  ;; internal procedures
  ;; representation of poly
  (define (foldr start op seq)
    (if (null? seq)
      start 
      (foldr (op start (car seq)) op (cdr seq))))

  (define (make-poly-from-list var term-list)
    (define (term order)
      (tag (make-poly var 
                      (make-dense order '(1)))))
    (define (mult t)
      (let ((order (car t))
            (coeff (cadr t)))
        (mul coeff (term order))))
    (define (empty) 
      (tag (make-poly var (the-empty-termlist))))
    (define (strip-tag p)
      (if (and (equal? 'polynomial (type-tag p))
               (equal? var (variable (contents p))))
        (contents p)
        (make-poly var (make-dense '0 (list p)))))
    (foldr (empty)
           add
           (map mult term-list)))

  (define (make-poly variable term-list)
    (cons variable (densify term-list)))

  (define (termlist->sparse L)
    (foldr (make-sparse '())
           join
           (terms L)))
  (define (termlist->dense L)
    (foldr (make-dense 0 '())
           join
           (terms L)))
  (define (densify L)
    (cond ((< (densness L) 1/4)
           (if (equal? (type-tag L) 'sparse)
             L
             (termlist->sparse L)))
          ((> (densness L) 1/2)
           (if (equal? (type-tag L) 'dense)
             L
             (termlist->dense L)))
          (else L)))

  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (same-variable? x y)
    (and (variable? x)
         (variable? y)
         (eq? x y)))
  (define variable? symbol?)

  ;; representation of terms and term lists
  (define (the-empty-termlist) (make-sparse '()))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (join term term-list)))

  ;; operations on polynomials
  (define (terms L)
    (if (empty-termlist? L)
      '()
      (cons (first-term L)
            (terms (rest-terms L)))))
  (define (max-order L)
    (apply max (map order (terms L))))
  (define (min-order L)
    (apply min (map order (terms L))))
  (define (count-nonzero L)
    (apply + (map (lambda (x) (if (=zero? x) 0 1))
                  (map coeff (terms L)))))
  (define (densness L)
    (if (empty-termlist? L)
      1
      (let ((span (inc (- (max-order L) 
                          (min-order L)))))
        (/ (count-nonzero L) span))))

  (define (value->poly var x)
    (make-poly var (make-dense 0 (list x))))

  (define (unify x y)
    (let* ((x-var (symbol->string (variable x)))
           (y-var (symbol->string (variable y)))
           (var (string->symbol (if (string<? x-var y-var) 
                                  x-var y-var))))
      (cond ((and (equal? var (variable x))
                  (equal? var (variable y)))
             (list x y))
            ((equal? var (variable x))
             (list x (value->poly var (tag y))))
            ((equal? var (variable y))
        (list (value->poly var (tag x)) y)))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error 'polynomial-package/add
             "cannot add polynomials, not in a same var ~a ~a"
             p1 p2)))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1)
                            (term-list p2)))
      (error 'polynomial-package/sub
             "cannot sub polynomials, not in the same var ~a ~a"
             p1 p2)))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error 'polynomial-package/mul
             "cannot mul polynomials, not in a same var ~a ~a"
             p1 p2)))

  (define (div-poly p1 p2)
    (define (poly terms)
      (make-poly (variable p1) terms))
    (if (same-variable? (variable p1)
                        (variable p2))
      (map poly (div-terms (term-list p1)
                           (term-list p2)))
      (error 'polynomial-package/div
             "cannot div polynomials, not in a same var ~a ~a"
             p1 p2)))
    
  (define (add-terms terms-1 terms-2)
    (define (op>=1 terms-1 terms-2)
      (let ((term-1 (first-term terms-1))
            (rest-1 (rest-terms terms-1))
            (term-2 (first-term terms-2))
            (rest-2 (rest-terms terms-2)))

        (cond ((order< term-1 term-2)
               (adjoin-term term-1
                            (op rest-1 terms-2)))
              ((order> term-1 term-2)
               (adjoin-term term-2 
                            (op terms-1 rest-2)))
              ((order= term-1 term-2)
               (adjoin-term (make-term
                              (order term-1)
                              (add (coeff term-1)
                                   (coeff term-2)))
                            (op rest-1 rest-2))))))

    (define (op terms-1 terms-2)
      (cond ((empty-termlist? terms-1)
             terms-2)
            ((empty-termlist? terms-2)
             terms-1)
            (else
             (op>=1 terms-1 terms-2))))

    (op terms-1 terms-2))
    
  (define (mul-term x terms)
    (if (empty-termlist? terms)
        terms 
        (let ((y (first-term terms))
              (rest (rest-terms terms)))
          (adjoin-term
           (make-term 
            (+ (order x) (order y))
            (mul (coeff x) (coeff y)))
           (mul-term x rest)))))

  (define (mul-terms terms-1 terms-2)
    (cond ((empty-termlist? terms-1)
            terms-1)
          ((empty-termlist? terms-2)
           terms-2)
          (else 
           (add-terms (mul-term (first-term terms-1)
                                terms-2)
                      (mul-terms (rest-terms terms-1)
                                 terms-2)))))

  (define (sub-terms L1 L2)
    (add-terms L1 (negate-termlist L2)))
  
  (define (max-term L)
    (if (empty-termlist? (rest-terms L))
      (first-term L)
      (max-term (rest-terms L))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) 
              (the-empty-termlist))
        (let ((t1 (max-term L1))
              (t2 (max-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let* ((new-c (div (coeff t1) 
                                 (coeff t2)))
                     (new-o (- (order t1) 
                                 (order t2)))
                     (new-term (make-term new-o new-c))
                     (deficit (mul-term new-term L2))
                     (result (div-terms (sub-terms L1 deficit) L2))
                     (quot (car result))
                     (rem (cadr result)))
                (list (adjoin-term new-term quot) rem))))))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
      (make-poly (variable p1)
                 (gcd-terms (term-list p1)
                            (term-list p2)))
      (error 'polynomial-package/gcd
             "cannot gcd polynomials, not in a same var ~a ~a"
             p1 p2)))

  (define (repr-term var term)
    (string-append 
      "[" (repr (coeff term)) "]" var "^" (number->string (order term))))

  (define (repr-termlist var terms)
    (cond ((empty-termlist? terms) 
           (string-append "[0]" var "^0"))
          ((=zero? (coeff (first-term terms)))
           (repr-termlist var (rest-terms terms)))
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

  (define (poly-zero? p)
    (empty-termlist? (term-list p)))

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

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (define (tag-simplify p)
    (cond ((empty-termlist? (term-list p)) 0)
          ((zero? (max-order (term-list p)))
           (coeff (first-term (term-list p))))
          (else
           (tag p))))

  (generics 'put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag-simplify (apply add-poly (unify p1 p2)))))
  (generics 'put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag-simplify (apply mul-poly (unify p1 p2)))))
  (generics 'put 'negate '(polynomial)
       (lambda (p) (tag-simplify (negate-poly p))))
  (generics 'put 'greatest-common-divisor '(polynomial polynomial)
    (lambda (x y)
      (map tag-simplify (apply gcd-poly (unify x y)))))
  (generics 'put 'variable '(polynomial)
       (lambda (p) (variable p)))
  (generics 'put 'sub '(polynomial polynomial)
       (lambda (x y) (tag-simplify (apply sub-poly (unify x y)))))
  (generics 'put 'div '(polynomial polynomial)
       (lambda (x y) (map tag-simplify (apply div-poly (unify x y)))))
  (generics 'put '=zero? '(polynomial) poly-zero?)
  (generics 'put 'repr '(polynomial) repr-poly)
  (generics 'put 'make-polynomial-from-termlist 'polynomial
         (lambda (var terms) 
           (tag (make-poly var terms))))
  (generics 'put 'make 'polynomial
       (lambda (var terms) 
         (make-poly-from-list var terms)))
  'done)

;; ======================================================
;; juggle package
;; ======================================================
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
  (juggle 'put 'juggle '(term dense)
    (lambda (x y)
      (list (term->dense x) y)))
  (juggle 'put 'juggle '(dense term)
    (lambda (x y)
      (list x (term->dense y))))
  (juggle 'put 'juggle '(sparse term)
    (lambda (x y)
      (list x (term->sparse y))))
  (juggle 'put 'juggle '(term sparse)
    (lambda (x y)
      (list (term->sparse x) y)))
  (juggle 'put 'juggle '(sparse dense)
    (lambda (x y)
      (list x (dense->sparse y))))
  (juggle 'put 'juggle '(dense sparse)
    (lambda (x y)
      (list (dense->sparse x) y)))
  'done)

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
  (if (has? args)
      (apply-strip args)
      (let ((juggled (apply juggle args)))
        (if (and juggled
                 (has? juggled))
          (apply-strip juggled)
          (method-not-found)))))


;; =====================================================
;; generics operations
;; =====================================================
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
(define (variable p) (apply-generic 'variable p))
(define (greatest-common-divisor a b) (apply-generic 'greatest-common-divisor a b))

(define (order t) (apply-generic 'order t))
(define (coeff t) (apply-generic 'coeff t))
(define (by key p?)
  (lambda (term-1 term-2)
    (p? (key term-1) (key term-2))))
(define (order< x y) ((by order <) x y))
(define (order<= x y) ((by order <=) x y))
(define (order> x y) ((by order >) x y))
(define (order>= x y) ((by order >=) x y))
(define (order= x y) ((by order =) x y))

(define (first-term x) (apply-generic 'first-term x))
(define (rest-terms x) (apply-generic 'rest-terms x))
(define (empty-termlist? x) (apply-generic 'empty-termlist? x))
(define (join x y) (apply-generic 'join x y))

;; =====================================================
;; constructors
;; =====================================================
(define (make-integer n) (apply-generic 'make-integer n))
(define (make-rational-number n d) (apply-generic 'make-rational n d))
(define (make-rational n d)
  ((generics 'get 'make 'rational) n d))
(define (make-real n) (apply-generic 'make-real n))
(define (make-complex-from-real-imag x y) 
  (apply-generic 'make-complex-from-real-imag x y))
(define (make-complex-from-mag-ang r a)
  (apply-generic 'make-complex-from-mag-ang r a))
(define (make-complex x y)
  (apply-generic 'make-complex x y))
(define (make-scheme-number x)
  ((generics 'get 'make 'scheme-number) x))
(define (make-polynomial var terms)
  ((generics 'get 'make 'polynomial) var terms))
(define (make-polynomial-from-termlist var terms)
  ((generics 'get 'make-polynomial-from-termlist 'polynomial) var terms))
(define (make-term order coeff)
  ((generics 'get 'make 'term) order coeff))
(define (make-dense order vals)
  ((generics 'get 'make 'dense) order vals))
(define (make-sparse lst)
  ((generics 'get 'make 'sparse) lst))
;; ======================================================
;; converters
;; ======================================================
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
(define (*->polynomial var x)
  (make-polynomial-from-termlist 
    var 
    (make-dense 0 (list x))))
(define (scheme-number->polynomial var x)
  (*->polynomial var x))
(define (number->polynomial var x)
  (*->polynomial var x))
(define (term->dense x)
  (make-dense (order x) (list (coeff x))))
(define (juggle . args)
  (define (tags args)
    (map type-tag args))
  (let ((proc (juggle-table 'get 'juggle (tags args))))
    (if proc
      (apply proc args)
    false)))
(define (term->sparse x)
  (make-sparse (list (list (order x) (coeff x)))))
(define (dense->sparse x)
  (if (empty-termlist? x)
    (make-sparse '())
    (join (term->sparse (first-term x))
          (dense->sparse (rest-terms x)))))
;; ======================================================
;; installed
;; ======================================================
(define generics (setup-table 'generics))
(install-scheme-number-package generics attach-tag)
(install-number-package generics attach-tag)
(install-term-package generics attach-tag)
(install-dense-package generics attach-tag)
(install-sparse-package generics attach-tag)
(install-polynomial-package generics attach-tag)
(install-rational-package generics attach-tag)

(define juggle-table (setup-table 'juggle))
(install-juggle-package juggle-table)
