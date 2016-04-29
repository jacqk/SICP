; 1.3.1 procedures as arguments
; p37 sum-integers

(define (sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

; p38 sum-cubes

(define (cube a)
  (* a a a))

(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))

; p38 pi-sum

(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1 (* a (+ a 2))) (pi-sum (+ a 4) b))))

; p38 term

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))

; 39 sum-cubes-2

(define (inc n)
  (+ n 1))

(define (sum-cubes-2 a b)
  (sum cube a inc b))

; 39 sum-integers-2

(define (identity x) x)

(define (sum-integers-2 a b)
  (sum identity a inc b))

; p39 pi-sum-2

(define (pi-sum-2 a b)
  (define (pi-term x)
    (/ 1
       (* x
          (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; integral

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

; ex1.29

(define (simpson f a b n)
  (define h
    (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (factor k)
    (cond ((or (= k 0) (= k n)) 1)
          ((odd? k) 4)
          (else 2)))
  (define (term k)
    (* (factor k) (y k)))
  (define (next k)
    (+ k 1))
  (if (not (odd? n))
    (* (/ h 3) (sum term (exact->inexact 0) next n))
    (display "error")))

(define (odd? x)
  (= (remainder x 2) 1))

; ex1.30

(define (sum term a next b)
  (define (sum-iter a result)
    (if (> a b)
      result
      (sum-iter (next a) (+ (term a) result))))
  (sum-iter a 0))

; ex1.31

(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

(define (factorial a)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (product identity 1 inc a))

(define (product-iter term a next b)
  (define (iter a prod)
    (if (> a b)
      prod
      (iter (next a) (* (term a) prod))))
  (iter a 1))

(define (factorial a)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (product-iter identity 1 inc a))

; 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (accumulate combiner null-value term (next a) next b) (term a))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate-iter + 0 term a next b))

(define (product term a next b)
  (accumulate-iter * 1 term a next b))

; 1.33

(define (filtered-accumulate combiner null-value term a next b filtering)
  (if (> a b)
    null-value
    (cond ((filtering a) (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filtering)))
          (else (filtered-accumulate combiner null-value term (next a) next b filtering)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))


(define (square x)
  (* x x))

(define (sum-prime a b)
  (define (identity y) y)
  (define (inc x)
    (+ x 1))
  (filtered-accumulate + 0 identity a inc b prime?))

(define (filtered-accumulate combiner null-value term a next b filtering)
  (if (> a b)
    null-value
    (cond ((filtering a b) (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filtering)))
          (else (filtered-accumulate combiner null-value term (next a) next b filtering)))))

(define (gcb a b)
  (if (= b 0)
    a
    (gcb b (remainder a b))))

(define (coprime? a b)
  (and (< a b)
       (= (gcb a b) 1)))

(define (sum-coprime b)
  (define (identity x)
    x)
  (define (inc x)
    (+ x 1))
  (filtered-accumulate * 1 identity 1 inc b coprime?))

