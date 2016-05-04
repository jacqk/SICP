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

(define (perimeter-rect rect)
  (* (+ (length-rect rect)
        (width-rect rect))
     2))

(define (area-rect rect)
  (* (length-rect rect)
     (width-rect rect)))

(define (length-rect rect)
  (segment-length (car rect)))

(define (width-rect rect)
  (segment-length (cdr rect)))

(define (segment-length segment)
  (let ((start-x (x-point (start-segment segment)))
        (start-y (y-point (start-segment segment)))
        (end-x (x-point (end-segment segment)))
        (end-y (y-point (end-segment segment))))
    (sqrt (+ (square (- start-x end-x))
             (square (- start-y end-y))))))

(define (make-rect p1 p2 p3 p4)
  (cons (make-segment p1 p2) (make-segment p1 p4)))

; test

(define p1 (make-point 1 1))
(define p2 (make-point 1 2))
(define p3 (make-point 2 2))
(define p4 (make-point 2 1))

(define rect1 (make-rect p1 p2 p3 p4))
(newline)
(display (perimeter-rect rect1))
(newline)
(display (area-rect rect1))

; 2.1.3 what is meant by data?

; (define (cons x y)
;   (define (dispatch m)
;     (cond ((= m 0) x)
;           ((= m 1) y)
;           (else (error "Argument not 0 or 1 -- CONS" m))))
;   dispatch)

; (define (car z) (z 0))
; (define (cdr z) (z 1))

; ex 2.4

; (define (cons x y)
;   (lambda (m) (m x y)))

; (define (car z)
;   (z (lambda (p q) p)))

; (define (cdr z)
;   (z (lambda (p q) q)))

; ex 2.5

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car c)
  (define (iter a result)
    (if (= (remainder a 2) 0)
      (iter (/ a 2) (+ result 1))
      result))
  (iter c 0))

; test
(newline)
(define test (cons 2 2))
(display (car test))

(define (cdr c)
  (define (iter a result)
    (if (= (remainder a 3) 0)
      (iter (/ a 3) (+ result 1))
      result))
  (iter c 0))


; test
(newline)
(display (cdr test))

; ex2.6

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (add-1 zero))

(define one (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))
