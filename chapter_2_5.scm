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
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

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
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))

; test
(install-scheme-number-package)
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
  (define (equ?-rat x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero? x)
    (= (numer x) 0))

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
  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))
  (put '=zero? '(rational)
       (lambda (x) (=zero? x)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define make-rational
  (lambda (n d) ((get 'make 'rational) n d)))

; test
(install-rational-package)
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

; rectangular-complex package

(define (install-rectangular-package)
  ;; internal procedure
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a))
          (* r (sin a))))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z) (sqrt (+ (square (real-part z))
                                 (square (imag-part z)))))
  (define (angle z) (atan (imag-part z) (real-part z)))

  ;;interface to rest of the system
  (define (tag z) (attach-tag 'rectangular z))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; run
(install-rectangular-package)

; polar-complex package

(define (install-polar-package)
  ;;internal procedure
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (make-from-mag-ang r a)
    (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  ;; interface to the rest of the system
  (define (tag z) (attach-tag 'polar z))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; run
(install-polar-package)

; complex-main
;
(define (install-complex-package)
  ;; internal procedure
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ?-complex-rec z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (equ?-complex-polar z1 z2)
    (and (= (magnitude z1) (magnitude z2))
         (= (angle z1) (angle z2))))
  (define (=zero? z)
    (and (= (real-part z) 0)
         (= (imag-part z) 0)))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (x y) (tag (make-from-mag-ang x y))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ?-complex-rec z1 z2)))
  (put '=zero? '(complex) 
       (lambda (z) (=zero? z)))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; test
(install-complex-package)
; (define e (make-complex-from-real-imag 1 2))
; (define f(make-complex-from-real-imag 2 4))
; (newline)
; (display e)
; (newline)
; (display f)
; (newline)
; (display (add f e))
; (newline)
; (display (sub e f))
; (newline)
; (display (mul f e))
; (newline)
; (display (div e f))

; ex2.77
; ex2.78

(define (type-tag datum)
  (cond ((number? datum) datum)
        ((pair? datum) (car datum))
        (else (error
                "Bad typed datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error
                "Bad typed datum -- TYPE-TAG" datum))))

(define (attach-tag tag datum)
  (cond ((number? datum) datum)
        (else (cons tag datum))))

; test
; (define ten (make-scheme-number 10))
; (display 10)

; ex2.79
; ex2.80

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
