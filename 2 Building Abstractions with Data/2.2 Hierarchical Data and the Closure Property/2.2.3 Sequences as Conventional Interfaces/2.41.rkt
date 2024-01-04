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

(define (permutations s)
  (if (null? s)   ; empty set?
      (list nil)  ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) 
                        (cons x p))
                      (permutations 
                       (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (sums-to k start s)
  (if (= k 1)         
    (if (<= start s) 
      (list (list s)) nil)
    (flatmap
      (lambda (i)                                  
        (map (lambda (p) (cons i p))               
             (sums-to (dec k) (inc i) (- s i))))  
      (enumerate-interval start s))))

(define (unordered-triples s)
  (sums-to 3 1 s))

(define (ordered-triples s)
  (flatmap permutations (unordered-triples s)))

(define (product seq . rst)
  (if (null? rst)
    (map list seq)
    (let ((prod-rst (apply product rst)))
      (flatmap (lambda (i) 
                 (map (lambda (p)
                        (cons i p)) prod-rst))
               seq))))

(define (triples s)
  (filter (lambda (triple)
            (let ((a (car triple))
                  (b (cadr triple))
                  (c (cdar triple)))
              (and (not (= a b)) 
                   (not (= b c))
                   (not (= a c))
                   (= (+ a b c) s))))
          (product (enumerate-interval 1 s)
                   (enumerate-interval 1 s)
                   (enumerate-interval 1 s))))
