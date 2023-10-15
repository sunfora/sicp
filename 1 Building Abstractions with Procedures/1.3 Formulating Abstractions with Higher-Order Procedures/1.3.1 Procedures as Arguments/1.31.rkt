#lang sicp

(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-product n)                               ; k = 1 2 3 4 5
  (define (even k) (* 2 k))                          ;     2 4 6 8 10
  (define (odd k) (dec (even k)))                    ;     1 3 5 7 9
  (define (term k) (/ (* (even (dec k)) (even k))    
                      (* (odd k) (odd k))))
  (* 4.0 (product term 2 inc (inc n))))
