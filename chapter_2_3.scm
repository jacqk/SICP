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


