;;; starts the function 
(define (sqrt x)
  
  ;;; returns true or false 
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))

  ;;; returns the average of two numbers
  (define (average x)
    (/ (+ x y) 2))
  
  ;;; returns a better guess 
  (define (improve guess)
    (average guess (/ x guess)))

  ;;; the iterator for the sqrt program
  (define (sqrt-iter guess)
    (if (good-enough? guess x) ;;; if guess^2 is approx. x with .001 variation, then guess is close enough to the real sqrt(x)
      guess ;;; return guess 
      (sqrt-iter (improve guess x) ;;; else run sqrt-iter on the improved guess and x again
                 x)))

  (sqrt-iter 1.0)
)



