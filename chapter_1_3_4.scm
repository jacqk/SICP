; 1.3.4 procedures as returned values
; p48 average dump

(define (average-damp f)
  (lambda (x) (average (f x) x)))

; p48 new square root & cube root

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

; p49 newton's method

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

(newline)
(display ((deriv cube) 5))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newton-method (lambda (y) (- (square y) x))
                  1.0))

; p50 abstractions and first-class procedures

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

; ex1.40

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newline)
(display (newton-method (cubic 3 2 1) 1))

; ex1.41

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

(newline)
(display (((double (double double)) inc) 5))

; ex1.42

(define (compose f g)
  (lambda (x) (f (g x))))

(newline)
(display ((compose square inc) 6))

; ex1.43

(define (repeated f k)
  (if (= k 1)
    f
    (compose f (repeated f (- k 1)))))

(newline)
(display ((repeated square 2) 5))

; ex1.44

(define (smooth f)
  (define (average a b c) (/ (+ a b c) 3.0))
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(newline)
(display ((smooth square) 5))

(define (smooth-n f n)
  (lambda (x) (((repeated smooth n) f) x)))

(newline)
(display ((smooth-n square 10) 5))

; ex1.45 unsolved

(define (expt base n)
  (cond ((= n 0) 1)
        ((odd? n) (* base (expt base (- n 1))))
        (else (expt (square base) (/ n 2)))))

; (newline)
; (display (expt 1 2))
; (display (expt 4 3))
; (display (expt 5 4))

(define (lg n)
  (cond ((> (/ n 2) 1) (+ 1 (lg (/ n 2))))
        ((< (/ n 2) 1) 0)
        (else 1)))

(define (n-average-damp f n)
  (repeated average-damp n) f)

(define (damp-nth-root n damp-times)
  (lambda (x) (fixed-point
                (n-average-damp
                  (lambda (y)
                    (/ x (expt y (- n 1))))
                  damp-times)
                1.0)))

(define (nth-root n)
  (damp-nth-root n (lg n)))

; ex1.46

(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
        (if (good-enough? guess next)
          next
          (try next))))
    (try first-guess)))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (good-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (improve guess) (f guess))
  ((iterative-improve good-enough? improve) first-guess))

(define (sqrt x)
  (define dx 0.00001)
  (define (good-enough? v1 v2) (< (abs (- v1 v2)) dx))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

