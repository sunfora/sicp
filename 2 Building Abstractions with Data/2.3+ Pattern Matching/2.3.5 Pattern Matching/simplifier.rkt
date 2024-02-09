#lang sicp

; ========= dsimp+ ==============

(define (dsimp+ expr)
  (canon (sort-expr (canon (dsimp expr)))))

; ========= dsimp =============== 

(define deriv-rules
  '(((dd (?c c) (? v)) 0)
    ((dd (?v v) (? v)) 1)
    ((dd (?v v) (? u)) 0)

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

(define dsimp
  (simplifier deriv-rules))

; ========= canon =============== 

(define canonical-form-rules
  '(
    ; compute constants
    (((? op) (?c x) (?c y))
     (:e (: (op x y))))
    ((+ (?c x) (+ (?c y) (? z)))
     (+ (:e (: (+ x y))) (: z)))
    ((* (?c x) (* (?c y) (? z)))
     (* (:e (: (* x y))) (: z)))

    ; simplify
    ((* 1 (? x)) (: x))
    ((* (? x) 1) (: x))
    ((+ 0 (? x)) (: x))
    ((+ (? x) 0) (: x))
    ((* 0 (? x)) 0)
    ((* (? x) 0) 0)

    ; expand sums
    ((* (? x) (+ (? y) (? z)))
     (+ (* (: x) (: y))
        (* (: x) (: z))))
    ((* (+ (? x) (? y)) (? z))
     (+ (* (: x) (: z))
        (* (: y) (: z))))
    
    ; move out constants
    ((+ (? x) (?c c))
     (+ (: c) (: x)))
    ((* (? x) (?c c))
     (* (: c) (: x)))
    ((+ (?v v) (+ (?c c) (? x)))
     (+ (: c) (+ (: v) (: x))))
    ((* (?v v) (* (?c c) (? x)))
     (* (: c) (* (: v) (: x))))

    ; unique association
    ((+ (+ (? x) (? y)) (? z))
     (+ (: x) (+ (: y) (: z))))
    ((* (* (? x) (? y)) (? z))
     (* (: x) (* (: y) (: z))))

    ; collect same terms 
    ((+ (? x) (? x))
     (* 2 (: x)))
    ((+ (? x) (+ (? x) (? y)))
     (+ (* 2 (: x)) (: y)))

    ((+ (* (?c a) (? x)) (? x))
     (* (:e (: (+ 1 a))) (: x)))
    ((+ (* (?c a) (? x)) (+ (? x) (? y)))
     (+ (* (:e (: (+ 1 a))) (: x)) (: y)))

    ((+ (? x) (* (?c a) (? x)))
     (* (:e (: (+ 1 a))) (: x)))
    ((+ (? x) (+ (* (?c a) (? x)) (? y)))
     (+ (* (:e (: (+ 1 a))) (: x)) (: y)))

    ((+ (* (?c a) (? x))
        (* (?c b) (? x)))
     (* (:e (: (+ a b)))
        (: x)))
    ((+ (* (?c a) (? x))
        (+ (* (?c b) (? x)) (? y)))
     (+ (* (:e (: (+ a b))) (: x))
        (: y)))
    ))

(define canon 
  (simplifier canonical-form-rules))

; ======== sort-expr ============

(define (expr->list op expr)
  (define (op? expr)
    (and (pair? expr)
         (eq? op (car expr))))
  (cond ((null? expr) expr)
        ((op? (caddr expr)) 
         (cons (cadr expr)
               (expr->list op (caddr expr))))
        (else (cdr expr))))

(define (list->expr op lst)
  (if (null? (cddr lst))
    (cons op lst)
    (list op 
          (car lst) 
          (list->expr op (cdr lst)))))

(#%require (only racket
                 symbol<?
                 sort))

(#%require (only racket/mpair
                 mlist->list 
                 list->mlist))

(define (sort-symbols lst)
  (list->mlist (sort (mlist->list lst) symbol<?)))


(define (sort-product product)
  (let* ((lst (expr->list '* product))                  
         (sorted (if (number? (car lst))
                   (cons (car lst)
                         (sort-symbols (cdr lst)))
                   (sort-symbols lst)))
         (expr (list->expr '* sorted)))
    expr))

(define (sort-products lst)
  (define (key product)
    (let ((result (expr->list '* product)))
      (if (number? (car result))
        (cdr result)
        result)))
  (list->mlist (sort (mlist->list 
                       (map sort-product lst))
                     lex<? 
                     #:key key)))

(define (lex<? product-1 product-2)
  (define (eq-size<? product-1 product-2)
    (and (not (null? product-1))
         (let ((x (car product-1))
               (y (car product-2))
               (xr (cdr product-1))
               (yr (cdr product-2)))
           (or (and (eq? x y)
                    (eq-size<? xr yr))
               (symbol<? x y)))))
  (cond ((< (length product-1) (length product-2))
         true)
        ((> (length product-1) (length product-2))
         false)
        (else (eq-size<? product-1 product-2))))

(define (sort-sum sum)
  (let* ((subs (expr->list '+ sum))
         (products (if (number? (car subs))
                     (cons (car subs)
                           (sort-products (cdr subs)))
                     (sort-products subs)))
         (result (list->expr '+ products)))
    result))

(define (sort-expr expr)
  (cond ((atom? expr) expr)
        ((eq? '+ (car expr))
         (sort-sum expr))
        ((eq? '* (car expr))
         (sort-product expr))
        (else 
          (error "unknown expr"))))


; =========== dict ==============

(define (fail dict)
  false)

(define (failed? dict) 
  (not dict))

(define (extend-dict pat value dict) 
  (let* ((name (variable-name pat))
         (p (assq name dict)))
    (cond ((not p) (cons (list name value) dict))
          ((equal? (cadr p) value) dict)
          (else (fail dict)))))

(define (lookup var dict)
  (if (failed? dict)
    (error "trying to evaluate failed dict"))
  (let ((value (assq var dict)))
    (if value
      (cadr value)
      var)))

(define empty-dictionary '())

; ========== matcher ============

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

(define (match-pred pred?)
  (lambda (pat exp dict)
    (if (pred? exp)
      (extend-dict pat exp dict)
      (fail dict))))

(define constant? number?)
(define expression? (lambda (x) true))
(define (variable? expr)
  (and (symbol? expr)
       (parsed-symbol? take-variable expr)))

(define match-constant (match-pred constant?))
(define match-expression (match-pred expression?))
(define match-variable (match-pred variable?))

; ====== parse variable =========

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
; ======= instantiator ==========
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

(define (evaluate expr)
  (eval expr
        (interaction-environment)))

(define (substitute expr dict)
  (define (look var)
    (lookup var dict))
  (car (tree-map look (list expr))))

; ======== simplifier ===========

(define (rule-pattern rule)
  (car rule))

(define (rule-skeleton rule)
  (cadr rule))

(define (simplifier the-rules)
  (define (simplify-expr expr)
    (walk try-rules expr))
  (define (try-rules expr)
    (define (scan rules)
      (if (null? rules)
        expr
        (let* ((rule (car rules))
               (pattern (rule-pattern rule))
               (skeleton (rule-skeleton rule))
               (dict (match pattern expr empty-dictionary)))
          (if (failed? dict)
            (scan (cdr rules))
            (simplify-expr (instantiate skeleton dict))))))
    (scan the-rules))
  simplify-expr)
; ========= little fun ==========

(define factorial-rules
  '(((* (?c a) (?c b))
     (:e (* (: a) (: b))))
    
    ((f 0) 1)
    ((f (?c n)) (* (: n) (f (:e (dec (: n))))))))

(define fact-simp
  (simplifier factorial-rules))

(define fib-rules 
  '(((+ (?c a) (?c b))
     (:e (+ (: a) (: b))))
    
    ((f 0) 0)
    ((f 1) 1)
    ((f (?c n)) (+ (f (:e (- (: n) 1)))
                   (f (:e (- (: n) 2)))))))

(define fib-simp
  (simplifier fib-rules))
