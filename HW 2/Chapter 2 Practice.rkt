; two ways to define the make-rat fucntion
; this way does not reduce the value to lowest term upon creation
(define (make-rat n d) (cons n d))
; this does create a reduced number when the number is created
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; get the numerator of the number
(define (numer x) (car x))
; get the denominator of the number 
(define (denom x) (cdr x))

; print the number 
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; add, subtract, multiply, and divide the number 
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

; test if two numbers are equal 
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


