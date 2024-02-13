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
