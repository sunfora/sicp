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
       (eq? '+ (cadr e))))

(define addend              
  ;Addend of the sum e.
  car)

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
        (else (list a1 '+ a2))))

(define (product? e)           
  ;Is e a product?
  (and (pair? e) 
       (eq? '* (cadr e))))

(define multiplier          
  ;Multiplier of the product e.
  car)

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
        (else (list m1 '* m2))))

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
        (else (error "unknown expression 
                      type: DERIV" exp))))
