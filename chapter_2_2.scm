; ch2.2 hierarchical data and the closure property
; 2.2.1 representing sequences

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

; test
(define squares (list 1 4 9 16 25))
(newline)
(display (list-ref squares 3))

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
      count
      (length-iter (cdr a) (+ count 1))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

; test
(define odds (list 1 3 5 7))
(newline)
(display (length odds))
(newline)
(display (append squares odds))

; ex2.17
(define (last-pair items)
  (list-ref items (- (length items) 1)))

; test
(newline)
(display (last-pair odds))
(newline)
(display (last-pair squares))

; ex2.18

(define (reverse items)
  (define (iter a b)
    (if (null? a)
      b
      (iter (cdr a) (cons (car a) b))))
  (iter items ()))

; test
(newline)
(display (reverse (list 1 4 9 16 25)))

; ex2.19

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values)))))

(define (no-more? items)
  (null? items))
(define (except-first-denomination items)
  (cdr items))
(define (first-denomination items)
  (car items))

; test
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(newline)
(display (cc 100 us-coins))
(newline)
(display (cc 100 (reverse us-coins)))

; ex2.20

(define (same-parity a . b)
  (define (same x y)
    (= (remainder x 2) (remainder y 2)))
  (define (iter items prod)
    (cond ((null? items) prod)
          ((same a (car items)) (iter (cdr items) (cons (car items) prod)))
          (else (iter (cdr items) prod))))
  (iter b (list a)))

; test
(newline)
(display (same-parity 1 2 3 4 5 6 7))
(newline)
(display (same-parity 2 3 4 5 6 7))

(define (scale-list items factor)
  (if (null? items)
    ()
    (cons (* (car items) factor)
          (scale-list (cdr items) factor))))

; test
(newline)
(display (scale-list (list 1 2 3 4 5) 10))

(define (map proc items)
  (if (null? items)
     ()
     (cons (proc (car items))
           ((map proc (cdr items))))))

; test
(display (map abs (list -10 2.5 -11.6 17)))
; (display (map (lambda (x) (* x x))
;              (list 1 2 3 4)))



