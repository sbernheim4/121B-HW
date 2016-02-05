; Samuel Bernheim (bernheim@brandeis.edu) 2016-2-4

; Exercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; cdr definition 
(define (cdr z)
  (z (lambda (p q) q)))