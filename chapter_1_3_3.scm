; ch 1.3.3 procedures as general methods
;
; p44 finding roots of equations by the halfinterval method

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value) (search f neg-point midpoint))
              ((negative? test-value) (search f midpoint pos-point))
              (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y)
  (/ (+ x y) 2.0))

; add check for f(a) and f(b) as they shall not on the same side
;
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (positive? a-value) (negative? b-value))
           (search f b a))
          ((and (positive? b-value) (negative? a-value))
           (search f a b))
          (else
            (error "Values are not of opposite sign" a b)))))

(newline)
(display (half-interval-method sin 2.0 4.0))
(newline)
(display (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                               1.0
                               2.0))

; p46 finding fixed points of functions

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        guess
        (try next))))
  (try first-guess))

(newline)
(display (fixed-point cos 1.0))
(newline)
(display (fixed-point (lambda (x) (+ (sin x) (cos x)))
                      1.0))

; square root

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(newline)
(display (sqrt 2))

; ex1.35

(newline)
(display (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

; ex1.36

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        guess
        (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x)))
                      2.0)

(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             2.0)

; ex1.37

(define (cont-frac n d k)
  (define (iter j)
    (if (= j k)
      (/ (n k) (d k))
      (/ (n j) (+ (d j) (iter (+ j 1))))))
  (iter 1))

(newline)
(display (cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    11))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (< i 1)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(newline)
(display (cont-frac-iter (lambda (i) 1.0)
                         (lambda (i) 1.0)
                         11))

; ex1.38

(define (e k)
  (+ (cont-frac-iter
       (lambda (i) 1.0)
       (lambda (i)
         (cond ((= (remainder i 3) 2) (* 2 (/ (+ i 1) 3)))
               (else 1)))
       k)
     2.0))

(newline)
(display (e 100))

; ex1.39

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (square x)))
             (lambda (i) (- (* 2 i) 1))
             k))

(newline)
(display (tan-cf 3.14 100))
