#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (collect branch)
    (cond ((leaf? branch) '())
          ((memq symbol (symbols (left-branch branch)))
           (cons 0 (collect (left-branch branch))))
          (else 
           (cons 1 (collect (right-branch branch))))))

  (if (memq symbol (symbols tree))
    (collect tree)
    (error "not a member:" symbol (symbols tree))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge-simple trees)
  (cond ((null? trees) 
         (error "huffman tree cannot be empty"))
        ((null? (cdr trees))
         (car trees))
        (else 
         (successive-merge-simple 
           (adjoin-set (make-code-tree 
                         (car trees)
                         (cadr trees))
                       (cddr trees))))))

(define (successive-merge-linear trees)
  ; add with infinity represented as false
  (define (+* a b)
    (cond ((not a) a)
          ((not b) b)
          (else (+ a b))))
  ; compare with infinity represented as false
  (define (<=* a b)
    (or (not b)
        (and a b
             (<= a b))))
  ; call weight or produce infinity
  (define (weight* tree)
    (if tree
      (weight tree)
      false))
  ; 
  ; to put it simply: 
  ;  1. take either first two from first queue
  ;  2. first two from second queue
  ;  3. or take first from both
  ; 
  ; follow the case weight of which is lower
  ; in the end push the value to the second queue
  ; 
  ; proceed until only one element in queue-2 is present
  ; 
  (define (iter queue-1 queue-2)
    (let ((from-first  (+* (weight* (first queue-1))
                           (weight* (second queue-1))))
          (from-second (+* (weight* (first queue-2))
                           (weight* (second queue-2))))
          (from-both   (+* (weight* (first queue-1))
                           (weight* (first queue-2)))))
      (cond ((and (empty-queue? queue-1)
                  (empty-queue? (drop queue-2)))
             (first queue-2))
            ((and (<=* from-first from-second)
                  (<=* from-first from-both))
             (iter (drop (drop queue-1))
                   (push (make-code-tree 
                           (first queue-1)
                           (second queue-1))
                         queue-2)))
            ((and (<=* from-second from-first)
                  (<=* from-second from-both))
             (iter queue-1
                   (push (make-code-tree 
                           (first queue-2)
                           (second queue-2))
                         (drop (drop queue-2)))))
            ((and (<=* from-both from-first)
                  (<=* from-both from-second))
             (iter (drop queue-1)
                   (push (make-code-tree 
                           (first queue-1)
                           (first queue-2))
                         (drop queue-2)))))))
  (cond ((null? trees)
         (error "huffman tree cannot be empty"))
        ((null? (cdr trees))
         (car trees))
        (else (iter (list->queue (cddr trees))
                    (list->queue (list (make-code-tree 
                                         (car trees)
                                         (cadr trees))))))))

(define successive-merge successive-merge-simple)
; (define successive-merge successive-merge-linear)

(define (make-queue inbox outbox)
  (if (null? outbox)
    (list '() (reverse inbox))
    (list inbox outbox)))

(define (inbox queue)
  (car queue))
(define (outbox queue)
  (cadr queue))

(define (push v queue)
  (make-queue
    (cons v (inbox queue))
    (outbox queue)))

(define (queue->list queue)
  (append (outbox queue)
          (reverse (inbox queue))))

(define (list->queue lst)
  (make-queue '() lst))

(define (empty-queue? queue)
  (and (null? (inbox queue))
       (null? (outbox queue))))

(define (first queue)
  (if (empty-queue? queue)
    false
    (car (outbox queue))))

(define (second queue)
  (first (drop queue)))

(define (drop queue)
  (if (empty-queue? queue) 
    queue
    (make-queue (inbox queue)
                (cdr (outbox queue)))))

;==== tests & miscelanious ====

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define generated 
  (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

(if (equal? generated sample-tree)
  (begin (display "[ok] : trees are equal")
         (newline))
  (error "trees are not equal"))

(define freq-max 999)

(#%require (only racket/mpair
                  mlist->list
                  list->mlist))
(#%require (only racket
                 sort))

(define (random-leafs n)
  (define (random-freqs-unsorted n)
    (if (zero? n)
      '()
      (cons (make-leaf n (inc (random freq-max)))
            (random-freqs-unsorted (dec n)))))
  (let ((freqs (random-freqs-unsorted n)))
     (list->mlist (sort (mlist->list freqs) #:key weight <))))

(define (random-test n tree-builder)
  (define leafs (random-leafs n))
  (define start (runtime))
  (tree-builder leafs)
  (- (runtime) start))

(define (random-tests n tree-builder)
  (define times 100)
  (define (time) (random-test n tree-builder))

  (do ((i 1 (inc i))
       (t 0 (+ t (time))))
    ((< times i) (exact->inexact (/ t times)))))

(define (make-table start stop step)
  (define (entry fn)
      (number->string
        (random-tests start fn)))
  (let ((simple (entry successive-merge-simple))  
        (linear (entry successive-merge-linear))
        (generation (number->string start)))
    (if (>= start stop)
      ""
      (string-append
        generation "\t\t" simple "\t\t" linear "\n"
        (make-table (+ start step) stop step)))))

(define (show-stats)
  (display (string-append 
             "gen\t\tsimple\t\tlinear\n"
             (make-table 1 10 1) 
             (make-table 10 100 10) 
             (make-table 100 1000 100)
             (make-table 1000 5001 1000))))

(show-stats)
