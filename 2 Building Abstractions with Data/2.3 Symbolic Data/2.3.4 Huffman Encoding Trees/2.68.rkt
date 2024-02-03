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

(#%require (only racket 
                 with-handlers 
                 exn-message
                 exn:fail?))

(define (expect-error thunk)
  (let ((message (with-handlers ([exn:fail? exn-message]) 
                   (thunk) 
                   false)))
    (if message
      (begin (display "[ok] : ") 
             (display message) 
             (newline))
      (error "error expected"))))

(if (equal? 
      (encode (decode sample-message sample-tree) sample-tree)
      sample-message)
  (begin (display "[ok] : messages are equal")
         (newline))
  (error "messages are not equal"))

(expect-error 
  (lambda ()
    (let ((decoded (decode sample-message sample-tree)))
      (encode (cons 'E decoded) sample-tree))))
