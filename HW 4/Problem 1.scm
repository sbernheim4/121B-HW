;; This is the code for Streams and Lazy Evaluation



(define-syntax cons-stream
 (syntax-rules ()
   ((cons-stream a b)
    (cons a (delay b)))))

(define head car)
(define (tail s) (force (cdr s)))
(define stream-car car)
(define stream-cdr tail)
