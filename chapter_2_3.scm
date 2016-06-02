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

; (define (equal? list1 list2)
;   (cond ((and (null? list1) (null? list2)) #t)
;         ((and (pair? (car list1)) (pair? (car list2)))
;          (and (equal? (car list1) (car list2))
;               (equal (cdr list1) (cdr list2))))
;         ((not (or (pair? (car list1)) (pair? (car list2))))
;          (and (eq? (car list1) (car list2))
;               (equal? (cdr list1) (cdr list2))))
;         (else #f)))

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

; ex2.58 a, b

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

(define (sum? expr)
  (and (pair? expr) (eq? (cadr expr) '+)))

(define (addend s) (car s))

(define (augend s) (list-ref s 3))

(define (product? expr)
  (and (pair? expr) (eq? (cadr expr) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (list-ref p 3))

; ex2.58 c unsolved

; 2.3.3 example: representing sets
; sets as unordered lists

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) ())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; ex2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (cons (car set1) set2)))))
; test

(define list-1 '(x y z w))
(define list-2 '(a b x y))

(newline)
(display list-1)
(display list-2)

(newline)
(display (element-of-set? 'x list-1))
(newline)
(display (adjoin-set 's list-1))
(newline)
(display (intersection-set list-1 list-2))
(newline)
(display (union-set list-1 list-2))

; ex2.60

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) ())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; test
(newline)
(display (element-of-set? 'x list-1))
(newline)
(display (adjoin-set 's list-1))
(newline)
(display (union-set list-1 list-2))
(newline)
(display (intersection-set list-1 list-2))

; sets as ordered lists

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((> x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    ()
    (let ((x1 (car set1))
          (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((> x1 x2)
             (intersection-set set1 (cdr set2)))))))

; ex2.61

(define (adjoin-set x set)
  (cond ((= x (car set)) set)
        ((> x (car set))
         (cons (car set) (adjoin-set x (cdr set))))
        ((< x (car set))
         (cons x set))))

; ex2.61

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((> x1 x2)
                       (cons x2 (union-set set1 (cdr set2))))
                      ((< x1 x2)
                    (cons x1 (union-set (cdr set1) set2))))))))

; test

(define list-1 (list 1 2 3 4))
(define list-2 (list 3 4 5 6))

(newline)
(display (element-of-set? 1 list-1))
(newline)
(display (element-of-set? 3 list-2))
(newline)
(display (intersection-set list-1 list-2))
(newline)
(display (adjoin-set 2.5 list-1))
(newline)
(display (union-set list-1 list-2))

; sets as binary trees

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x () ()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-tree set))
                    (right-tree set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-tree set)
                    (adjoin-set x (right-tree set))))))

; ex2.63

(define (tree->list-1 tree)
  (if (null? tree)
    ()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree ()))

; ex2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons () elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

; ex2.65

(define (intersection-tree set1 set2)
  (list->tree
    (intersection-set (tree->list-2 set1)
                      (tree->list-2 set2))))

(define (union-tree set1 set2)
  (list->tree
    (union-set (tree->list-2 set1)
               (tree->list-2 set2))))

; test
(define tree-1
  (list 7 (list 3 (list 1 () ()) (list 5 () ())) (list 9 () (list 11 () ()))))

(define tree-2
  (list 3 (list 1 () ()) (list 7 (list 5 () ()) (list 9 () (list 11 () ())))))

(define tree-3
  (list 5 (list 3 (list 1 () ()) ()) (list 9 (list 7 () ()) (list 11 () ()))))

(newline)
(display (element-of-set? 3 tree-1))
(newline)
(display (element-of-set? 100 tree-2))
(newline)
(display (tree->list-1 tree-1))
(newline)
(display (tree->list-1 tree-2))
(newline)
(display (tree->list-1 tree-3))
(newline)
(display (tree->list-2 tree-1))
(newline)
(display (tree->list-2 tree-2))
(newline)
(display (tree->list-2 tree-3))
(newline)
(display (list->tree (list 1 3 5 7 9 11)))
(newline)
(display (union-tree tree-1 tree-2))
(newline)
(display (intersection-tree tree-1 tree-2))
(newline)

; p109

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key set-of-records))
         (car set-of-records))
        (else (lookup fiven-key (cdr set-of-records)))))

; ex2.66

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key set-of-records))
         (entry set-of-records))
        ((> given-key (key set-of-records))
         (lookup given-key (right-branch set-of-records)))
        ((< given-key (key set-of-records))
         (lookup given-key (left-branch set-of-records)))))

; 2.3.4 example: Huffman encoding trees

; generating huffman trees
; representing huffman trees

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf object)
  (cadr object))

(define (weight-leaf object)
  (caddr object))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

; the decoding procedure

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      ()
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

; sets of weighted elements

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    ()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
                             (cadr pair))
                  (make-leaf-set (cdr pairs))))))

; ex2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(newline)
(display sample-tree)
(newline)
(display sample-message)
(newline)
(display (leaf? (make-leaf 'A 1)))
(newline)
(display (symbol-leaf (make-leaf 'A 1)))
(newline)
(display (weight-leaf (make-leaf 'A 1)))
(newline)
(display (left-branch sample-tree))
(newline)
(display (right-branch sample-tree))
(newline)
(display (symbols sample-tree))
(newline)
(display (weight sample-tree))
(newline)
(display (decode sample-message sample-tree))

; ex2.68

(define (encode message tree)
  (if (null? message)
    ()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (choose symbol tree)
    (if (symbol-in-tree? symbol (left-branch tree))
      '0
      '1))
  (if (symbol-in-tree? symbol tree)
    (cond ((leaf? tree) ())
          (else
            (cond ((eq? (choose symbol tree) '0)
                   (cons '0
                         (encode-symbol symbol (left-branch tree))))
                  ((eq? (choose symbol tree) '1)
                   (cons '1
                         (encode-symbol symbol (right-branch tree)))))))))

(define (symbol-in-tree? symbol tree)
  (define (item-in-list list- item)
    (cond ((null? list-) #f)
          ((eq? item (car list-)) #t)
          (else (item-in-list (cdr list-) item))))
  (let ((symbols (symbols tree)))
    (item-in-list symbols symbol)))

; test

(define text '(a d a b b c a))
(newline)
(display (encode text sample-tree))

; ex2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge ordered-set)
  (cond ((= 0 (length ordered-set)) ())
        ((= 1 (length ordered-set)) (car ordered-set))
        (else (let ((new-sub-tree (make-code-tree (car ordered-set)
                                                  (cadr ordered-set)))
                    (remained-set (cddr ordered-set)))
                (successive-merge (adjoin-set new-sub-tree remained-set))))))

(newline)
(define sample-list '((A 4) (B 2) (C 1) (D 1)))
(display sample-list)
(newline)
(display (generate-huffman-tree sample-list))

; ex2.70

(define rock-list '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(define rock-huffman-tree (generate-huffman-tree rock-list))

(define rock-msg '(Get a job
                   Sha na na na na na na na na
                   Get a job
                   Sha na na na na na na na na
                   Wah yip yip yip yip yip yip yip yip yip
                   Sha boom))

(newline)
(display (encode rock-msg rock-huffman-tree))

; ex2.71

(define ex-list-1 '((a 1) (b 2) (c 4) (d 8) (e 16)))

(define ex-list-2 '((a 1) (b 2) (c 4) (d 8) (e 16) (f 32) (g 64) (h 128) (i 256) (j 512)))

; ex2.72


