(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n) ;;; if the (test-divisor)^2 > n then return n: aka n is the largest divisor of n
        ((divides? test-divisor n) test-divisor) ;;; if n is divisible by the test-divisor return the the test-divisor 
        (else (find-divisor n (+ test-divisor 1))) ;;; otherwise see if n+1 is a divisor of n
  )
)

(define (square x) (* x x))

;;; if the remainder of a/b is 0 then a divides b 
(define (divides? a b)
  (= (remainder b a) 0))

;;; a number is prime if the smallest divisor is itself 
(define (prime? n)
  (= n (smallest-divisor n)))