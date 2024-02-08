#lang sicp

(define deriv-rules
  '(
    ((dd (?c c) (? v)) 0)
    ((dd (?v v) (? v)) 1)
    ((dd (?v u) (? u)) 0)

    ((dd (+ (? a) (? b)) (? v)) 
     (+ (dd (: a) (: v))
        (dd (: b) (: v))))

    ((dd (- (? a) (? b)) (? v))
     (- (dd (: a) (: v))
        (dd (: b) (: v))))

    ((dd (* (? a) (? b)) (? v))
     (+ (* (dd (: a) (: v))
           (: b))
        (* (: a)
           (dd (: b) (: v)))))))

;(define dsimp
;  (simplifier deriv-rules))

(define (fail dict)
  false)

(define (failed? dict) 
  (not dict))

(define (arbitrary-constant? pat)
  (eq? '?c (car pat)))

(define (arbitrary-variable? pat)
  (eq? '?v (car pat)))

(define (arbitrary-expression? pat)
  (eq? '? (car pat)))

(define (variable-name pat)
  (cadr pat))

(define (atom? x)
  (not (pair? x)))

(define (match pat exp dict)
  (cond ((failed? dict) dict)
        ((atom? pat)
         (match-atomic pat exp dict))
        ((arbitrary-constant? pat)
         (match-constant pat exp dict))
        ((arbitrary-variable? pat)
         (match-variable pat exp dict))
        ((arbitrary-expression? pat)
         (match-expression pat exp dict))
        ((atom? exp)
         (fail dict))
        (else 
         (match (car pat)
                (car exp)
                (match (cdr pat)
                       (cdr exp)
                       dict)))))

(define (match-atomic pat exp dict)
  (if (eq? pat exp)
    dict
    (fail dict)))

(define (extend-dict pat value dict) 
  (let* ((name (variable-name pat))
         (p (assq name dict)))
    (cond ((not p) (cons (list name value) dict))
          ((equal? (cadr p) value) dict)
          (else (fail dict)))))

(define (match-pred pred?)
  (lambda (pat exp dict)
    (if (pred? exp)
      (extend-dict pat exp dict)
      (fail dict))))

(define constant? number?)
; (define variable? symbol?)
(define expression? (lambda (x) true))

(define match-constant (match-pred constant?))

(define match-expression (match-pred expression?))

(define (delimiter? char)
  (if (member char '(#\- #\_)) true false))

(define (parse-failed? result)
  (not (cadr result)))

(define (parse-fail result)
  (list (car result)
        false))

(define (make-result parsed rest)
  (list parsed rest))

(define (parse-parsed result)
  (car result))

(define (parse-rest result)
  (cadr result))

(define (take-pred pred? chars)
  (cond ((and (pair? chars)
              (pred? (car chars)))
         (let* ((result (take-pred pred? (cdr chars)))
                (parsed (parse-parsed result))
                (rest (parse-rest result)))
           (make-result (inc parsed) rest)))
        (else (make-result 0 chars))))

(define (zero-parsed?=>parse-fail parser)
  (lambda (chars)
    (let* ((result (parser chars))
           (parsed (parse-parsed result)))
      (if (or (parse-failed? result)
              (zero? parsed))
        (parse-fail result)
        result))))

(define take-word
  (zero-parsed?=>parse-fail 
    (lambda (chars)
      (take-pred char-alphabetic? chars))))

(define take-delim
  (zero-parsed?=>parse-fail 
    (lambda (chars)
      (take-pred delimiter? chars))))

(define (take-variable chars)
  (let* ((word-result (take-word chars))
         (word-rest   (parse-rest word-result))
         (delim-result  (take-delim word-rest))
         (delim-rest (parse-rest delim-result)))
    (if (not (parse-failed? delim-result))
      (take-variable delim-rest)
      word-result)))

(define (parsed-string? parser str)
  (let* ((chars (string->list str))
         (result (parser chars))
         (rest (parse-rest result)))
    (null? rest)))

(define (parsed-symbol? parser symbol)
  (parsed-string? parser (symbol->string symbol)))

(define (variable? expr)
  (and (symbol? expr)
       (parsed-symbol? take-variable expr)))

(define match-variable (match-pred variable?))

(define (walk f tree)
  (define (walk-f tree) (walk f tree))
  (cond ((null? tree) tree)
        ((list? tree) (f (map walk-f tree)))
        (else (f tree))))

(define (tree-map f tree)
  (walk (lambda (tree)
          (if (list? tree)
            tree
            (f tree)))
        tree))

(define (instantiate skeleton dict)
  (define (transform-nosplice s)
    (cond ((skeleton-evaluation? s) 
           (evaluate (skeleton-expr s)))
          ((skeleton-substitution? s)
           (substitute (skeleton-expr s) dict))
          (else s)))  

  (define (listify result)
    (if (skeleton-splice? result)
      (skeleton-expr result)
      (list result)))

  (define (delistify s)
    (if (atom? s)
      s
      (apply append s)))

  (define (transform s)
    (listify 
      (transform-nosplice 
        (delistify s))))

  (let ((result (walk transform skeleton)))
    (if (null? (cdr result))
      (car result)
      result)))

(define (skeleton-splice? skeleton)
  (and (pair? skeleton)
       (eq? ':@ (car skeleton))))

(define (skeleton-evaluation? skeleton)
  (and (pair? skeleton)
       (eq? ':e (car skeleton))))

(define (skeleton-substitution? skeleton)
  (and (pair? skeleton)
       (eq? ': (car skeleton))))

(define (skeleton-expr skeleton)
  (if (not (null? (cddr skeleton)))
    (error "expected one argument for skeleton" skeleton))
  (cadr skeleton))

(define (lookup var dict)
  (if (failed? dict)
    (error "trying to evaluate failed dict"))
  (let ((value (assq var dict)))
    (if value
      (cadr value)
      var)))

(define (evaluate expr)
  (display expr) (newline)
  (eval expr
        (interaction-environment)))

(define (substitute expr dict)
  (define (look var)
    (lookup var dict))
  (car (tree-map look (list expr))))

(define or-set 
  (instantiate '(:e (lambda (x)
                      (or (:@ (:e (map (lambda (n) (list 'equal? n 'x))
                                     (list (:@ (: x)))))))))
             '((x ("what" "the" "hell" 1)))))
