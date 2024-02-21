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

;; type data primitives

(define (attach-tag type-tag contents)
  (cond ((and (eq? type-tag 'scheme-number)
              (number? contents))
         contents)
        ((and (eq? type-tag 'scheme-number)
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

;; operations

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (make-scheme-number n)
  ((generics 'get 'make 'scheme-number) n))

(define (make-rational n d)
  ((generics 'get 'make 'rational) n d))

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

(define (exp x y) 
  (apply-generic 'exp x y))

(define (mul-3 a b c)
   (apply-generic 'mul-3 a b c))
;; coercion
(define (scheme-number->complex n)
  (make-complex-from-real-imag 
   (contents n) 0))
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(coercion 'put 'scheme-number 'complex 
          scheme-number->complex)

(coercion 'put 'scheme-number 'scheme-number
          scheme-number->scheme-number)

(coercion 'put 'complex 'complex 
          complex->complex)
;; packages 

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (generics 'put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (generics 'put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (generics 'put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (generics 'put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (generics 'put 'equ? '(scheme-number scheme-number) =)
  (generics 'put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (generics 'put 'make 'scheme-number
       (lambda (x) (tag x)))

  (generics 'put 'exp '(scheme-number scheme-number) 
            (lambda (x y) (tag (expt x y))))
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
  (define (mul-3 a b c)
    (make-from-mag-ang
      (* (magnitude a) (magnitude b) (magnitude c))
      (+ (angle a) (angle b) (angle c))))

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
  (generics 'put 'mul-3 
            '(complex complex complex)
            (lambda (a b c)
              (tag (mul-3 a b c))))
  (generics 'put 'real-part '(complex) real-part)
  (generics 'put 'imag-part '(complex) imag-part)
  (generics 'put 'magnitude '(complex) magnitude)
  (generics 'put 'angle '(complex) angle)

  'done)

;; installed packages
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)


