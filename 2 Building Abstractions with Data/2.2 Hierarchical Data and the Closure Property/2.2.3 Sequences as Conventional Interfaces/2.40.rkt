#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (filter p seq)
  (accumulate 
    (lambda (x y)          
       (if (p x)             
         (cons x y)         
         y))                 
     nil                      
     seq))         

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (inc low) high))))

(define (unique-pairs n)
  (flatmap                                      
   (lambda (i)                                  
     (map (lambda (j)                           
            (list i j))                         
          (enumerate-interval 1 (dec i))))      
   (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime? x)
  (define (iter i)
    (if (<= (* i i) x)
      (if (zero? (remainder x i))
        #false
        (iter (inc i)))
      #true))
  (if (> x 1)
    (iter 2)
    #false))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (unique-pairs n))))
