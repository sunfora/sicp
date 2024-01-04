#lang sicp
(#%require sicp-pict)
(#%require racket/base)

(define (below p1 p2)
  (rotate90 (beside (rotate270 p1)
                    (rotate270 p2))))
