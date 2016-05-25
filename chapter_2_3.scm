; 2.3 symbolic data
; 2.3.1 quotation p96

(define a 1)
(define b a)
(newline)
(display (list a b))
(display (list 'a 'b))

(car '(a b c))
(cdr '(a b c))

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; test
(newline)
(display (memq 'apple '(pear banana prune)))
(newline)
(display (memq 'apple '(x (apple sauce) y apple pear)))

; ex2.53

(newline)
(display (list 'a 'b 'c))
(newline)
(display (list (list 'george)))
(newline)
(display (cdr '((x1 x2) (y1 y2))))
(newline)
(display (cadr '((x1 x2) (y1 y2))))
(newline)
(display (pair? (car '(a short list))))
(memq 'red '((red shoes) (blue socks)))
(newline)
(memq 'red '(read shoes blue socks))

; ex2.54

(define (equal? list1 list2)
  (cond ((and (null? list1) (null? list2)) #t)
        ((and (pair? (car list1)) (pair? (car list2)))
         (and (equal? (car list1) (car list2))
              (equal (cdr list1) (cdr list2))))
        ((not (or (pair? (car list1)) (pair? (car list2))))
         (and (eq? (car list1) (car list2))
              (equal? (cdr list1) (cdr list2))))
        (else #f)))

; test
(newline)
(display (equal? '(this is a list) '(this is a list)))
(display (equal? '(this is a list) '(this (is a) list)))

; ex2.55

(newline)
(display (car ''abracadabra))
(newline)
(display ''abracadabra)

; 2.3.2 example: symbolic differentiation
; ex2.56
; ex2.57

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
           (make-product (deriv (multiplier expr) var)
                         (multiplicand expr))))
        ((exponentiation? expr)
         (make-product (exponent expr)
                       (make-product
                         (make-exponentiation (base expr)
                                              (make-sum (exponent expr) -1))
                         (deriv (base expr) var))))
        (else
          error "unknown expression type -- DERIV" expr)))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 . a2)
  (if (single-operand? a2)
    (let ((a2 (car a2)))
      (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2))
             (+ a1 a2))
            (else (list '+ a1 a2))))
    (append (list '+ a1) (cdr (make-sum (car a2) (cdr a2))))))

(define (=number? expr num)
  (and (number? expr) (= expr num)))

(define (make-product m1 . m2)
  (if (single-operand? m2)
    (let ((m2 (car m2)))
      (cond ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2))
             (* m1 m2))
            (else (list '* m1 m2))))
    (append (list '* m1) (cdr (make-product (car m2) (cdr m2))))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (addend s) (cadr s))

(define (augend s)
  (if (single-operand? (cddr s))
    (list-ref s 2)
    (apply make-sum (cddr s))))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (single-operand? (cddr p))
    (list-ref p 2)
    (apply make-product (cddr p))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (base expr)
  (cadr expr))

(define (exponent expr)
  (caddr expr))

(define (exponentiation? expr)
  (and (pair? expr) (eq? (car expr) '**)))

(define (single-operand? exp)
  (eq? (cdr exp) ()))

; test
(newline)
(display (deriv '(+ 3 x) 'x))
(newline)
(display (deriv '(* x y) 'x))
(newline)
(display (deriv '(* (* x y) (+ x 3)) 'x))
(newline)
(display (deriv '(** x y) 'x))
(newline)
(display (deriv '(+ x y z) 'x))
(newline)
(display (deriv '(* x y z) 'x))
(newline)
(display (deriv '(* x y (+ x 3)) 'x))

; ex2.58

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (numbrer? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m2)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))


