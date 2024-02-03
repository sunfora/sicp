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
