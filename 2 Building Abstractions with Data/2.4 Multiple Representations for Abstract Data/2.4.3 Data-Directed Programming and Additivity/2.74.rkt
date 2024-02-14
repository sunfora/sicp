#lang sicp

(#%require racket/path)
(#%require racket/runtime-path)
(#%require racket/mpair)
(#%require (only racket
                 void
                 build-path
                 file->list
                 make-hash
                 hash-set!
                 hash-ref))
(#%require db)

(define-runtime-path *runtime-path* "2.74.rkt")
(define *runtime-dir* (path-only *runtime-path*))

(define *generics-table*
  (make-hash))

(define (put method type value)
  (let ((key (cons method type)))
    (if (not (hash-ref *generics-table* key false))
      (hash-set! *generics-table* key value)
      (error 'put
             "method ~a for type ~a already defined"
             method type))))

(define (get method type)
  (let ((result (hash-ref 
                  *generics-table* 
                  (cons method type) 
                  false)))
    (if (not result)
      (error 'get 
             "method ~a for type ~a does not exist"
             method type))
    result))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error 'type-tag 
             "bad tagged datum ~a" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error 'contents 
             "bad tagged datum ~a" datum)))

(define (get-record employee file)
  (let* ((division (type-tag file))
         (records (contents file))
         (get-record* (get 'get-record division))
         (record (get-record* employee records)))
    (if record 
      (attach-tag division record)
      record)))

(define (get-salary record)
  (let* ((division (type-tag record))
         (data (contents record))
         (get-salary* (get 'get-salary division)))
      (get-salary* data)))

(define (find-employee-record employee files)
  (apply append 
         (map (lambda (file)                             
                (let ((result (get-record employee file)))
                  (if result (list result) '())))
              files)))

(define (load-file division)
  (attach-tag division ((get 'load-file division))))

(define (unload-file file)
  ((get 'unload-file (type-tag file)) (contents file)))

(define (install-natural-schemes-package)
  (define dbpath
    (build-path *runtime-dir*                
                "assets/NaturalSchemes.list"))

  (define (convert lst)
    (map list->mlist (list->mlist lst)))

  (define (load-file)
    (convert (file->list dbpath)))

  (define (unload-file file)
    (void))

  (define get-record assoc)
  (define get-salary cadr)

  (put 'load-file 'natural-schemes load-file)
  (put 'unload-file 'natural-schemes unload-file)
  (put 'get-record 'natural-schemes get-record)
  (put 'get-salary 'natural-schemes get-salary)

  'done)

(define (install-xr-company-package)
  (define dbpath
    (build-path *runtime-dir*                
                "assets/xr-company.db"))

  (define (convert lst)
    (map list->mlist (list->mlist lst)))

  (define (load-file)
    (sqlite3-connect #:database dbpath))

  (define (unload-file file)
    (disconnect file))

  (define (get-record employee file)
    (let ((result (query-maybe-row 
                    file 
                    "SELECT id, name, phone, salary, address 
                     FROM employees WHERE name=? LIMIT 1" 
                    employee)))
      (if result
        (vector->list result)
        result)))

  (define get-salary cadddr)

  (put 'load-file 'xr-company load-file)
  (put 'unload-file 'xr-company unload-file)
  (put 'get-record 'xr-company get-record)
  (put 'get-salary 'xr-company get-salary)

  'done)


(install-natural-schemes-package)
(install-xr-company-package)

(define schemes (load-file 'natural-schemes))
(define xr (load-file 'xr-company))

(define companies
  (list schemes xr))

(get-salary (get-record "Сидоров Тимур Михайлович" schemes)) 
(get-salary (get-record "Glen Dale" xr))
(get-salary (get-record "Susan Bridge" xr))
(get-record "Qusan Bridge" xr)
(get-salary (car (find-employee-record "Andrew Bridson" companies)))

(for-each unload-file companies)

