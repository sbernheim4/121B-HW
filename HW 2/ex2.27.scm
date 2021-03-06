; Samuel Bernheim (bernheim@brandeis.edu) 2016-2-25

; This procedure gets the last element in a list by recursively calling cdr on a list until the list is empty
; The last element in the list is returned. 
(define (get-last param)
  (define (get-last-iter param last)
  (if (null? param)
      last
      (get-last-iter (cdr param) (car param))))
  (get-last-iter param 0))

; the size of the list is found by adding 1 to a counter variable every time the the size-iter loop is run
(define (size l)
  (define (size-iter l count)
    (if (null? l)
        count
        (size-iter (cdr l) (+ 1 count))))
  (size-iter l 0)
)

; the list is returned in reverse order
(define (reverse x)
    (define (reverse-iter orig new)
      (let ((l (get-last orig)))
      (if (= (size x) (size new))
          new
          (reverse-iter (cdr orig) (append (list (car orig)) new))))
      )
  (reverse-iter x (list ))
)


; the list is reversed with each sublist also in revserse order
(define (deep-reverse x)
  (define (deep-reverse-iter orig new)
    (if (= (size x) (size new)) ; when the size of the new list is the size of the input list, the reversing is done
        new
        (deep-reverse-iter (cdr orig) (reverse (append new (list (reverse (car orig))))))
        ))
  (deep-reverse-iter x (list))
)