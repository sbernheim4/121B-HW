; Samuel Bernheim (bernheim@brandeis.edu) 2016-1-30

;;; Exercise 1.44
(define (smooth f dx) ; two parameters f and dx
  ; returns ((f(x) + f(x+dx) + (fx-dx)) / 3) when a second lambda parameter is passed 
  (lambda (x) (average (f x) (f (+ x dx)) (f (- x dx)))) 
)

(define (square x) (* x x))
(define (average a b c) (/ (+ a b c) 3))

(define (repeated func n x)
  (if (= n 0)
      x
      (repeated func (- n 1) (func x))
  )
)