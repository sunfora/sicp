#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound i)
  (min (car i) (cdr i)))

(define (upper-bound i)
  (max (car i) (cdr i)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (define (pos? x) (<= 0 x))
  
  (define x- (lower-bound x))
  (define x+ (upper-bound x))
  (define y- (lower-bound y))
  (define y+ (upper-bound y))
  
  (define (signs? a b c d)
    (and (eq? a (pos? x-))
         (eq? b (pos? x+))
         (eq? c (pos? y-))
         (eq? d (pos? y+))))
  
  (define (++++?) (signs? #t #t #t #t))
  (define (-+++?) (signs? #f #t #t #t))
  (define (--++?) (signs? #f #f #t #t))

  (define (++-+?) (signs? #t #t #f #t))
  (define (-+-+?) (signs? #f #t #f #t))
  (define (---+?) (signs? #f #f #f #t))

  (define (++--?) (signs? #t #t #f #f))
  (define (-+--?) (signs? #f #t #f #f))
  (define (----?) (signs? #f #f #f #f))

  (define (++++) 
    (make-interval (* x- y-)  
                   (* x+ y+)))
  (define (-+++) 
    (make-interval (* x- y+)
                   (* x+ y+)))
  (define (--++) 
    (make-interval (* x- y+) 
                   (* x+ y-)))

  (define (++-+) 
    (make-interval (* y- x+)
                   (* y+ x+)))
  (define (-+-+) 
    (make-interval (min (* x- y+) (* x+ y-)) 
                   (max (* x- y-) (* x+ y+))))
  (define (---+) 
    (make-interval (* x- y+) 
                   (* x- y-)))

  (define (++--) 
    (make-interval (* y- x+) 
                   (* y+ x-)))
  (define (-+--) 
    (make-interval (* y- x+) 
                   (* y- x-)))
  (define (----) 
    (make-interval (* x+ y+) 
                   (* x- y-)))

  (cond
    ((++++?) (++++))
    ((-+++?) (-+++))
    ((--++?) (--++))
                   
    ((++-+?) (++-+))
    ((-+-+?) (-+-+))
    ((---+?) (---+))
                   
    ((++--?) (++--))
    ((-+--?) (-+--))
    ((----?) (----))))

(define (interval->string x)
  (string-append "["
                 (number->string (lower-bound x))
                 ", "
                 (number->string (upper-bound x))
                 "]"))

(define (div-interval x y)
  (define (revert t)
    (if (not (= 0 t))
      (/ 1.0 t)
      (error (string-append "cannot divide by interval: " (interval->string y)))))
  (mul-interval x 
                (make-interval 
                 (revert (upper-bound y)) 
                 (revert (lower-bound y)))))

(define (sub-interval x y)
  (add-interval x
                (make-interval
                  (* -1 (upper-bound y))
                  (* -1 (lower-bound y)))))

(define (width x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

(define (print-interval x)
  (display (interval->string x))
  (newline))

(define (eq-interval? a b)
  (and (= (lower-bound a) (lower-bound b))
       (= (upper-bound a) (upper-bound b))))

(define (test)
  (define (test n x y z)
    (let ((x^ (apply make-interval x))
          (y^ (apply make-interval y))
          (z^ (apply make-interval z)))
      (let ((r^ (mul-interval x^ y^)))
        (if (eq-interval? r^ z^)
          (display (string-append "[Test " (number->string n) "]: OK \n"))
          (error   (string-append "[Test " (number->string n) "]: Failed \n"
                                  "Expected: \n" 
                                      (interval->string x^) " * " (interval->string y^) 
                                      " = " (interval->string z^) "\n"
                                  "But got: " (interval->string r^)))))))
    (test 1 '( 1  2) '( 3  4) '( 3  8))
    (test 2 '(-1  2) '( 3  4) '(-4  8))
    (test 3 '(-2 -1) '( 3  4) '(-8 -3))
    (test 4 '( 1  2) '(-3  4) '(-6  8))
    (test 5 '(-1  2) '(-3  4) '(-6  8))
    (test 6 '(-2 -1) '(-3  4) '(-8  6))
    (test 7 '( 1  2) '(-4 -3) '(-8 -3))
    (test 8 '(-1  2) '(-4 -3) '(-8  4))
    (test 9 '(-2 -1) '(-4 -3) '( 3  8))
    (display "Tests passed. \n"))

(test)

(define (gen-test n a b c d)
  (define (interval x)
    (display "'(")
    (display (lower-bound x))
    (display " ")
    (display (upper-bound x))
    (display ")"))
  (let ((x (make-interval a b))
        (y (make-interval c d)))
    (display "(test ")
    (display n) (display " ")
    (interval x) (display " ")
    (interval y) (display " ")
    (interval (mul-interval x y)) (display ")\n")))

(define (gen-tests)
  (gen-test 1 +1 +2 +3 +4) ;++ ++
  (gen-test 2 -1 +2 +3 +4) ;-+ ++
  (gen-test 3 -1 -2 +3 +4) ;-- ++
  (gen-test 4 +1 +2 -3 +4) ;++ -+
  (gen-test 5 -1 +2 -3 +4) ;-+ -+
  (gen-test 6 -1 -2 -3 +4) ;-- -+
  (gen-test 7 +1 +2 -3 -4) ;++ --
  (gen-test 8 -1 +2 -3 -4) ;-+ --
  (gen-test 9 -1 -2 -3 -4) ;-- --
  )
