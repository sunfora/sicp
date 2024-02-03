#lang sicp

(define variable?           
  ; Is e a variable?
  symbol?)

(define (same-variable? v1 v2) 
  ;Are v1 and v2 the same variable?
  (and (variable? v1) (variable? v2)
       (eq? v1 v2)))

(define (^list p?)
  (lambda (e)
    (if (p? e)
      e
      (list e))))

(define (foldr start op lst)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter start lst))

(define (or-pred . preds)
  (lambda (e)
    (foldr #f 
           (lambda (result p?)
             (or result
                 (p? e)))
           preds)))

(define (cut-last lst)
  (define (iter without rest)
    (if (null? (cdr rest))
      (list (reverse without) (car rest))
      (iter (cons (car rest) without)
            (cdr rest))))
  (iter nil lst))

(define (join-by . rst)
  (let ((x (cut-last rst)))
    (let ((preds (car x))
          (elems (cadr x)))
      (apply append
             (map (^list (apply or-pred preds)) 
                  elems)))))

(define (complement-memq obj expr)
  (define (iter result expr)
    (cond ((null? expr) #f) 
          ((eq? obj (car expr)) result)
          (else (iter (cons (car expr)
                            result)
                      (cdr expr)))))
  (reverse (iter nil expr)))

(define (car-if-1 lst)
  (if (null? (cdr lst))
    (car lst)
    lst))

(define (first-operand operation)
  (lambda (expression)
    (car-if-1 (complement-memq operation
                               expression))))

(define (second-operand operation)
  (lambda (expression)
    (car-if-1 (cdr (memq operation
                         expression)))))

(define (has-operation? operation expression)
  (if (and (pair? expression)
           (memq operation expression))
    #t #f))

(define (sum? e)
  ;Is e a sum?
  (has-operation? '+ e))

(define addend             
  ;Addend of the sum e.
  (first-operand '+))

(define augend              
  ;Augend of the sum e.
  (second-operand '+))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  ;Construct the sum of a1 and a2.
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (join-by sum? 
                       product? 
                       (list a1 '+ a2)))))

(define (product? e)           
  ;Is e a product?
  (and (not (sum? e))
       (has-operation? '* e)))

(define multiplier          
  ;Multiplier of the product e.
  (first-operand '*))

(define multiplicand 
  ;Multiplicand of the product e.
  (second-operand '*))

(define (make-product m1 m2)   
  ;Construct the product of m1 and m2.
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (join-by product? 
                       (list m1 '* m2)))))

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
