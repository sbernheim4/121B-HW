(define x (list (list 1 2) (list 3 4)))

(define (fringe tree lst)
  (let ((last-element (get-last tree)))
    
  ))

(define (get-last param)
  (define (get-last-iter param last)
  (if (null? param)
      last
      (get-last-iter (cdr param) (car param))))
  (get-last-iter param 0))


(define (size l)
  (define (size-iter l count)
    (if (null? l)
        count
        (size-iter (cdr l) (+ 1 count))))
  (size-iter l 0)
)