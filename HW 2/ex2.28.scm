(define x (list (list 1 2) (list 3 4)))

(define (fringe tree lst)
    (define new-lst (append lst (car tree)))
    (fringe (cdr tree) new-lst))


(define (size l)
  (define (size-iter l count)
    (if (null? l)
        count
        (size-iter (cdr l) (+ 1 count))))
  (size-iter l 0)
)