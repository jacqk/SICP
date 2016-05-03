; 2.1 introduction to data abstraction
; ch2.1.1 example: arithmetic operations for rational numbers

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* numer x) (* denom y)
               (* numer y) (* denom x))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Pairs (cons)

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

; (define make-rat cons)
; (define numer car)
; (define denom cdr)

; representing rational numbers

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  )

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

; ex2.1

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (> d 0)
      (cons (/ n g) (/ d g))
      (cons (- (/ n g)) (- (/ d g))))))

; ch2.1.2 abstraction barriers

; ex 2.2

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment a b) (cons a b))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (midpoint-segment segment)
  (make-point (average (x-point (start-segment segment))
                       (x-point (end-segment segment)))
              (average (y-point (start-segment segment))
                       (y-point (end-segment segment)))))

(define (average a b)
  (/ (+ a b) 2))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; test
(define p1 (make-point 1 2))
(define p2 (make-point 3 4))
(define lineone (make-segment p1 p2))
(print-point (midpoint-segment lineone))


; ex2.3


