; Samuel Bernheim (bernheim@brandeis.edu) 2016-1-30

;;; Exercise 1.17
(define (double n) (* n 2) )

(define (halve n) (/ n 2))

(define (fast-mult a b)
  (cond ((= a 0) 0)
        ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))
  )
)