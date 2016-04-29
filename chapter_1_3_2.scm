; construsting procedures using lambda
; p41 lambda example

(lambda (x) ( + x 4))

(lambda (x) (/ 1.0 ( * x ( + x 2))))

; p41 pi-sum with lambda

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 ( * x ( + x 2))))
       a
       (lambda (x) (+ x 4))
       b))

; p41

(define (integral f a b dx)
  (* dx (sum f
             (+ a (/ dx 2.0))
             (lambda (x) (+ x dx))
             b)))

(define (cube x)
  (* x x x))

; p41 lambda & define

(define (plus4 x) (+ x 4))

(define plus4 (lambda (x) (+ x 4)))

; p42 use as procedure

((lambda (x y z) (+ x y (square z))) 1 2 3)

(define (square x)
  (* x x))

; using let to create local variables

; solution 1

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

; solution 2 with lambda

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

; solution 3 with let

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

; 1.34

(define (f g)
  (g 2))

; (f square)   ->    (square 2)  ->   4
; (f (lambda (z) (* z (+ z 1))))   ->   ((lambda (z) (* z (+ z 1))) 2)  ->  (* 2 (+ 2 1))  -> 6
; (f f)  ->   (f 2)  ->   (2 2)   ->   The object 2 is not applicable
