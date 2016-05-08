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
          (map proc (cdr items)))))

; test
(newline)
(display (map abs (list -10 2.5 -11.6 17)))
(display (map (lambda (x) (* x x))
             (list 1 2 3 4)))

(define (scale items factor)
  (map (lambda (x) (* x factor))
       items))

; ex2.21

(define (square-list items)
  (map (lambda (x) (* x x))
       items))

(define (square-list items)
  (if (null? items)
    ()
    (cons (square (car items))
          (square-list (cdr items)))))

; test
(newline)
(display (square-list (list 1 2 3 4)))

; ex2.22

(define (square-list items)
  (define (iter items prod)
    (if (null? items)
      (reverse prod)
      (iter (cdr items)
            (cons (square (car items))
                  prod))))
  (iter items ()))

(newline)
(display (square-list (list 1 2 3 4)))

; ex2.23

(define (for-each proc items)
  (if (not (null? items))
    (begin
      (proc (car items))
      (for-each proc (cdr items)))))

; test
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

; 2.2.2

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (count-leaves items)
  (cond ((null? items) 0)
        ((not (pair? items)) 1)
        (else (+ (count-leaves (car items))
                   (count-leaves (cdr items))))))

; test
(define x (cons (list 1 2) (list 3 4)))
(newline)
(display (length x))
(newline)
(display (count-leaves x))

; ex2.24

(newline)
(display (list 1 (list 2 (list 3 4))))

; ex2.25

(newline)
(define a (list 1 3 (list 3 7) 9))
(display a)
(newline)
(display (car (cdr (list-ref a 2))))
(newline)
(define b (list (list 7)))
(display b)
(newline)
(display (car (car b)))
(newline)
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(display c)
(newline)
(display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c)))))))))))))

; ex2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(newline)
(display (append x y))
(newline)
(display (cons x y))
(newline)
(display (list x y))

; ex2.27

(define (deep-reverse items)
  (cond ((null? items) ())
        ((pair? items) (reverse (cons  (deep-reverse (car items))
                                       (deep-reverse (cdr items)))))
        (else items)))

; test
(define x (list (list 1 2) (list 3 4)))
(newline)
(display (reverse x))
(newline)
(display (deep-reverse x))

; ex 2.28

(define (fringe items)
  (cond ((null? items) ())
        ((not (pair? items)) (list items))
        (else (append (fringe (car items))
                    (fringe (cdr items))))))

; test
(define x (list (list 1 2) (list 3 4)))
(newline)
(fringe x)

(fringe (list x x))

; ex2.29

(define (make-mobile left right)
  (list left right))
(define (make-branch len structure)
  (list len structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight items)
  (+ (branch-weight (left-branch items))
     (branch-weight (right-branch items))))

(define (branch-weight branch)
  (if (pair? (branch-structure branch))
    (total-weight (branch-structure branch))
    (branch-structure branch)))

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (mobile-balance items)
  (and (= (branch-torque (left-branch items))
          (branch-torque (right-branch items)))
       (branch-balance (left-branch items))
       (branch-balance (right-branch items))))

(define (branch-balance branch)
  (if (pair? (branch-structure branch))
    (mobile-balance (branch-structure branch)
    #t)))

; new comprehension

(define (make-mobile left right)
  (cons left right))
(define (make-branch len structure)
  (cons len structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))


; test
(define mobile (make-mobile (make-branch 10 25)
                            (make-branch 5 20)))
(define mobile (make-mobile (make-branch 5 mobile)
                            (make-branch 10 20)))

(newline)
(display (left-branch mobile))
(newline)
(display (right-branch mobile))
(newline)
(display (branch-length (left-branch mobile)))
(newline)
(display (branch-structure (right-branch mobile)))
(newline)
(display (total-weight mobile))
(newline)
(display (branch-torque (right-branch mobile)))
(newline)
(display (branch-torque (left-branch mobile)))
(newline)
(display (mobile-balance mobile))

; mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) ())
        ((not (pair? tree)) (* factor tree))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (scale-tree sub-tree factor)
           (* factor sub-tree)))
       tree))

; test

(newline)
(display (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10))

; ex2.30

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (* sub-tree sub-tree)))
       tree))

(define (square-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

; ex 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map proc sub-tree)
           (proc sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map (lambda (x) (* x x)) tree))

; test
(newline)
(display (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))

; ex 2.32

(define (subsets s)
  (if (null? s)
    (list ())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; test
(newline)
(display (subsets (list 1 2 3)))

; 2.2.3 sequences as conventional interfaces


