#lang sicp

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (f-coord f coord)
  (lambda (v . vs)
    (apply f (map coord (cons v vs)))))

(define (vect* t)
  (lambda (v . rst)
    (define (t-coord coord)           
      (apply (f-coord t coord) (cons v rst)))
    (make-vect (t-coord xcor-vect) 
               (t-coord ycor-vect))))

(define add-vect (vect* +))
(define sub-vect (vect* -))

(define (scale-vect s v)
  (define (*s x) (* s x))
  (make-vect ((f-coord *s xcor-vect) v)
             ((f-coord *s ycor-vect) v)))