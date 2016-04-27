; Core library for sls
; Implements sls library syntax and library procedures in terms of the
; core syntax and procedures

; Core syntax:
; 4.1.1: <variable>
; 4.1.2: quote
; 4.1.3: <operator>
; 4.1.4: lambda
; 4.1.5: if
; 4.1.6: set!
; 4.2.6: quasiquote
; 4.3.1: let-syntax
; 4.3.2: letrec-syntax
; 5.2:   define
; 5.3:   define-syntax
; 6.3.2: c[ad]{2,}r
;        R5RS only specifies c[ad]{2,4}r, but if we make them part of the
;        syntax we can support arbitrary-length specifiers. This amuses me,
;        so I shall do it.

; Core procedures:
; 6.1:   eqv? eq?
; 6.2.5: number? = < > <= >= + * - quotient remainder modulo
; 6.3.2: pair? cons car cdr
; 6.4:   procedure? apply

; Library syntax
; Copied from R5RS

; 4.2.1: cond
(define-syntax cond
 (syntax-rules (else =>)
  ((cond (else result1 result2 ...))
   (begin result1 result2 ...))
  ((cond (test => result))
   (let ((temp test))
    (if temp (result temp))))
  ((cond (test => result) clause1 clause2 ...)
   (let ((temp test))
    (if temp
     (result temp)
     (cond clause1 clause2 ...))))
  ((cond (test)) test)
  ((cond (test) clause1 clause2 ...)
   (let ((temp test))
    (if temp
     temp
     (cond clause1 clause2 ...))))
  ((cond (test result1 result2 ...))
   (if test (begin result1 result2 ...)))
  ((cond (test result1 result2 ...)
    clause1 clause2 ...)
   (if test
    (begin result1 result2 ...)
    (cond clause1 clause2 ...)))))

; 4.2.1: case
(define-syntax case
 (syntax-rules (else)
  ((case (key ...)
    clauses ...)
   (let ((atom-key (key ...)))
    (case atom-key clauses ...)))
  ((case key
    (else result1 result2 ...))
   (begin result1 result2 ...))
  ((case key
    ((atoms ...) result1 result2 ...))
   (if (memv key '(atoms ...))
    (begin result1 result2 ...)))
  ((case key
    ((atoms ...) result1 result2 ...)
    clause clauses ...)
   (if (memv key '(atoms ...))
    (begin result1 result2 ...)
    (case key clause clauses ...)))))

; 4.2.1: and
(define-syntax and
 (syntax-rules ()
  ((and) #t)
  ((and test) test)
  ((and test1 test2 ...)
   (if test1 (and test2 ...) #f))))

; 4.2.1: or
(define-syntax or
 (syntax-rules ()
  ((or) #f)
  ((or test) test)
  ((or test1 test2 ...)
   (let ((x test1))
    (if x x (or test2 ...))))))

; 4.2.2: let
(define-syntax let
 (syntax-rules ()
  ((let ((name val) ...) body1 body2 ...)
   ((lambda (name ...) body1 body2 ...)
    val ...))
  ((let tag ((name val) ...) body1 body2 ...)
   ((letrec ((tag (lambda (name ...)
                   body1 body2 ...)))
     tag)
    val ...))))

; 4.2.2: let*
(define-syntax let*
 (syntax-rules ()
  ((let* () body1 body2 ...)
   (let () body1 body2 ...))
  ((let* ((name1 val1) (name2 val2) ...)
    body1 body2 ...)
   (let ((name1 val1))
    (let* ((name2 val2) ...)
     body1 body2 ...)))))

; 4.2.2: letrec
(define-syntax letrec
 (syntax-rules ()
  ((letrec ((val1 init1) ...) body ...)
   (letrec "generate_temp_names"
    (var1 ...)
    ()
    ((var1 init1) ...)
    body ...))
  ((letrec "generate_temp_names"
    ()
    (temp1 ...)
    ((var1 init1) ...)
    body ...)
   (let ((var1 (make-undefined)) ...)
    (let ((temp1 init1) ...)
     (set! var1 temp1)
     ...
     body ...)))
  ((letrec "generate_temp_names"
    (x y ...)
    (temp ...)
    ((var1 init1) ...)
    body ...)
   (letrec "generate_temp_names"
    (y ...)
    (newtemp temp ...)
    ((var1 init1) ...) 
    body ...))))

; 4.2.3: begin
(define-syntax begin
 (syntax-rules ()
  ((begin exp ...)
   ((lambda () exp ...)))))

; 4.2.4: do
(define-syntax do
 (syntax-rules ()
  ((do ((var init step ...) ...)
    (test expr ...)
    command ...)
   (letrec
    ((loop
      (lambda (var ...)
       (if test
        (begin
         (if #f #f)
         expr ...)
        (begin
         command
         ...
         (loop (do "step" var step ...)
          ...))))))
    (loop init ...)))
  ((do "step" x)
   x)
  ((do "step" x y)
   y)))

; Library procedures

; 6.1:   equal?
(define (equal? a b)
 (cond
  ((and (null? a) (null? b)) #t)
  ((or  (null? a) (null? b)) #f)
  ((not (or (pair? a) (pair? b))) (eqv? a b))
  ((or  (pair? a) (pair? b)) #f)
  (else (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))))

; 6.2.5: zero? 
(define (zero? x) 
 (= x 0))

; 6.2.5: positive?
(define (positive? x) 
 (> x 0))

; 6.2.5: negative?
(define (negative? x) 
 (< x 0))

; 6.2.5: odd?
(define (odd? x) 
 (= (modulo x 2) 1))

; 6.2.5: even?
(define (even? x) 
 (= (modulo x 2) 0))

; 6.2.5: max
(define (max x . rst)
 (cond
  ((null? rst) x)
  ((> x (car rst)) (apply max (cons x (cdr rst))))
  (else (apply max rst))))

; 6.2.5: min
(define (min x . rst)
 (cond
  ((null? rst) x)
  ((< x (car rst)) (apply min (cons x (cdr rst))))
  (else (apply min rst))))

; 6.2.5: abs
(define (abs x)
 (if (positive? x) x (- x)))

; 6.2.5: gcd
(define (gcd . rst)
 (cond
  ((null? rst) 0)
  ((= (length rst) 1) (car rst))
  (else
   (let ((a (car rst)) (b (cadr rst)))
    (let loop ((a (max a b)) (b (min a b)))
     (if (zero? b)
      (apply gcd (cons a rst))
      (loop b (modulo a b))))))))

; 6.2.5: lcm
(define (lcm . rst)
 (cond
  ((null? rst) 1)
  ((= (length rst) 1) (car rst))
  (else (quotient (apply * rst) (apply gcd rst)))))

; 6.3.1: not
(define (not x)
 (case x
  ((#f) #t)
  (else #f)))

; 6.3.1: boolean?
(define (boolean? x)
 (case x
  ((#t #f) #t)
  (else #f)))

; 6.3.2: null?
(define (null? x)
 (case x
  (('()) #t)
  (else #f)))

; 6.3.2: list?
(define (list? x) 
 (and (pair? x) (list? (cdr x))))

; 6.3.2: list
(define (list . rst)
 rst)

; 6.3.2: length
(define (length x)
 (if (null? x) 0 (+ 1 (length (cdr x)))))

; 6.3.2: append
(define (append x . rst)
 (cond
  ((null? rst) x)
  ((null? x) (apply append rst))
  (else (cons (car x) (apply append (cons (cdr x) rst))))))

; 6.3.2: reverse
(define (reverse x)
 (if (null? x) x
  (let loop ((x x))
   (cond
    ((= (length x) 1) x)
    (else (append (loop (cdr x)) (list (car x))))))))

; 6.3.2: list-tail
(define (list-tail x k)
 (if (zero? k) x (list-tail (cdr x) (- k 1))))

; 6.3.2: list-ref
(define (list-ref x k)
 (car (list-tail x k)))

; 6.3.2: memq
(define (memq x xs)
 (cond
  ((null? xs) #f)
  ((eq?   x (car xs)) xs)
  (else (memq x (cdr xs)))))

; 6.3.2: memv
(define (memv x xs)
 (cond
  ((null? xs) #f)
  ((eqv?  x (car xs)) xs)
  (else (memv x (cdr xs)))))

; 6.3.2: member
(define (member x xs)
 (cond
  ((null? xs) #f)
  ((equal? x (car xs)) xs)
  (else (member x (cdr xs)))))

; 6.3.2: assq
(define (assq x xs)
 (cond
  ((null? xs) #f)
  ((eq? x (caar xs)) xs)
  (else (member x (cdr xs)))))

; 6.3.2: assv
(define (assv x xs)
 (cond
  ((null? xs) #f)
  ((eqv? x (caar xs)) xs)
  (else (member x (cdr xs)))))

; 6.3.2: assoc
(define (assoc x xs)
 (cond
  ((null? xs) #f)
  ((equal? x (caar xs)) xs)
  (else (member x (cdr xs)))))

; 6.4:   map
(define (map proc x . rst)
 (letrec ((firsts (lambda xs
                   (if (null? xs)
                    '()
                    (cons (caar xs) (apply firsts (cdr xs))))))
          (rests  (lambda xs
                   (if (null? xs)
                    '()
                    (cons (cdar xs) (apply rests  (cdr xs))))))
          (all-null? (lambda xs
                      (if (null? xs)
                       #t
                       (and (null? (car xs)) (apply all-null? (cdr xs)))))))
  (let loop ((xs (cons x rst)))
   (if (apply all-null? xs)
    '()
    (cons (apply proc (apply firsts xs)) (loop (apply rests xs)))))))
