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

(define (below p1 p2)
  (let ((upper (transform-painter p1 (make-vect 0.0 0.5)
                                     (make-vect 1.0 0.5)
                                     (make-vect 0.0 1.0)))
        (lower (transform-painter p2 (make-vect 0.0 0.0)
                                     (make-vect 1.0 0.0)
                                     (make-vect 0.0 0.5))))
    (lambda (frame)
      (upper frame)
      (lower frame))))
