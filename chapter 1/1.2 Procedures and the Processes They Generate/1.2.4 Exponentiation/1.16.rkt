(define (even? n)
  (= (remainder n 2) 0))

(define (fast-exp base p)
  (define (iter pb n result)
    (cond ((= 0 i) 
          result)
          ((even? n)
          (fast-exp (* pb pb) (/ n 2) result))
          (else
            (fast-exp pb (- n 1) (* pb result)))))
  (iter base p 1))

(fast-exp 2 4)

