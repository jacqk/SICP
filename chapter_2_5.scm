; chapter2.5 system with generic operations
;
; 2.5.1 generic arithmetic operations
;
(load "put-get.scm")

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types -- APPLY-GENERIC"
          (list op type-tags))))))

(define attach-tag cons)

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad typed datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad typed datum -- CONTENTS" datum)))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'sub x y))

; deal with scheme-number
;
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))

; test
; (install-scheme-number-package)
; (define a (make-scheme-number 4))
; (define b (make-scheme-number 2))
; (newline)
; (display a)
; (newline)
; (display (add a b))
; (newline)
; (display (sub a b))
; (newline)
; (display (mul a b))
; (newline)
; (display (div a b))

; deal with rational number
;
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (numer y) (denom x))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define make-rational
  (lambda (n d) ((get 'make 'rational) n d)))

; test
; (install-rational-package)
; (define c (make-rational 6 2))
; (define d (make-rational 1 2))
; (newline)
; (display (add c d))
; (newline)
; (display (sub c d))
; (newline)
; (display (mul c d))
; (newline)
; (display (div c d))

; deal with complex

(define (install-complex-package)
  ;; internal procedure
  ())
; 2.5.2 combining data of different dypes
;
; coercion
;
; hierarchies of types
;
; inadequacies of hierarchies
;
; 2.5.3 example: symbolic algebra
;
; arithmetic on polynomials
;
; representing term lists
;
; hierarchies of types in symbolic algebra
;
; extended exercise: rational functions
