; Samuel Bernheim (bernheim@brandeis.edu) 2016-1-30

;;; Exercise 1.43
(define (repeated func n x)
  (if (= n 0)
      x
      (repeated func (- n 1) (func x))
  )
)
