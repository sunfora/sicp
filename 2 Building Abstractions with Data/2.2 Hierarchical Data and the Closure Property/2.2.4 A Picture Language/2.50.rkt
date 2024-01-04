#lang sicp
(#%require sicp-pict)
(#%require racket/base)

(define (transform-painter 
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                  (vector-sub (m corner1) 
                              new-origin)
                  (vector-sub (m corner2)
                              new-origin)))))))
(define (flip-horiz p)
  (transform-painter p (make-vect 1.0 0.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0)))

(define (rotate180 p)
  (transform-painter p (make-vect 1.0 1.0)
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 0.0)))

(define (rotate270 p)
  (transform-painter p (make-vect 0.0 1.0)
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 0.0)))
