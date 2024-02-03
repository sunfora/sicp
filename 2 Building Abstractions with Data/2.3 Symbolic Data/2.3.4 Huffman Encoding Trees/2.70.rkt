#lang sicp

(#%require (only racket
                 string-upcase))

(#%require (only srfi/13
                 string-pad))

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

(define (successive-merge trees)
  (cond ((null? trees) 
         (error "huffman tree cannot be empty"))
        ((null? (cdr trees))
         (car trees))
        (else 
         (successive-merge
           (adjoin-set (make-code-tree 
                         (car trees)
                         (cadr trees))
                       (cddr trees))))))

(define (zip a b)
  (if (or (null? a)
          (null? b))
    '()
    (cons (list (car a) (car b))
          (zip (cdr a) (cdr b)))))

(define (generate-bits n)
  (define (c x)
    (lambda (y) (cons x y)))
  (cond ((>= 0 n) '())
        ((= 1 n) '((0) (1)))
        (else 
         (let ((less (generate-bits (dec n))))
           (append (map (c 0) less)
                   (map (c 1) less))))))

(define (fixed-length-encoder alphabet)
  (let* ((n (length alphabet))
         (bits (ceiling (log n 2))))
    (zip (map car alphabet)
         (generate-bits bits))))

(define (fixed-encode-symbol symbol encoder)
  (cond ((null? encoder) 
         (error "unnable to find: " symbol 'in encoder))
        ((equal? symbol (caar encoder))
         (cadar encoder))
        (else 
         (fixed-encode-symbol symbol (cdr encoder)))))

(define (fixed-encode text encoder)
  (define (encode-sym x)
    (fixed-encode-symbol x encoder))
  (apply append (map encode-sym text)))

(define alphabet
  '((A    2)  (NA  16)
    (BOOM 1)  (SHA  3)
    (GET  2)  (YIP  9)
    (JOB  2)  (WAH  1)))


(define (symbol-upcase x)
  (string->symbol 
    (string-upcase 
      (symbol->string x))))

(define message
  (map symbol-upcase
       '(Get a job
         Sha na na na na na na na na

         Get a job
         Sha na na na na na na na na

         Wah yip yip yip yip 
         yip yip yip yip yip
         Sha boom)))

(define (wrap text len)
  (define total (string-length text))
  (define (next x) (min total (+ len x)))

  (do ((start 0   (next start))
       (end   len (next end))
       (collect '() (cons
                      (substring text start end) 
                      collect)))
    ((= start end) (reverse collect))))

(define (bitstring bits)
  (apply string-append
               (map number->string bits)))

(define huffman-tree (generate-huffman-tree alphabet))
(define encoded-message (encode message huffman-tree))

(define fixed (fixed-length-encoder alphabet))
(define fixed-encoded-message (fixed-encode message fixed))

(define (display-encoding encoded-message type)
  (display "type: ") (display type) 
  (newline) (newline)

  (define before "encoded: ")

  (define wrap-length 16)
  (define wrapped (wrap (bitstring encoded-message)
                        wrap-length))
  
  (define padding (+ wrap-length (string-length before)))
  (define (prepare line)
    (string-append (string-pad line padding) "\n"))


  (define with-before (cons (string-append before
                                           (car wrapped))
                            (cdr wrapped)))

  (display (apply string-append (map prepare with-before)))
  (newline)


  (display "bits: ") (display (length encoded-message))
  (newline) (newline))

(display-encoding encoded-message "huffman")
(display-encoding fixed-encoded-message "fixed-length")
