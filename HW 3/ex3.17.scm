(define (contains? lst element)
  (cond ((null? lst) #f)
        ((eq? (car lst) element) #t)
        (else (contains? (cdr lst) element))))


(define (count-pairs lst)
  (good-count lst '() 0)
)

(define (good-count lst already-seen counter)
  (cond ((null? lst) counter)
        ((not (contains? already-seen (car lst))) (begin (set! already-seen (append already-seen (car lst) )) (good-count (cdr lst) already-seen (+ counter 1))))
        (else (good-count (cdr lst) already-seen (+ counter 1)))))

