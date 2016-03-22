;; This is the code for Streams and Lazy Evaluation

(define-syntax cons-stream
 (syntax-rules ()
   ((cons-stream a b)
    (cons a (delay b)))))

(define head car)
(define (tail s) (force (cdr s)))
(define stream-car car)
(define stream-cdr tail)

(define the-empty-stream (delay '()))
(define (empty-stream? s)
 (and (not (pair? s))
      (null? (force s))))

(define (1+ x) (+ 1 x))
(define (write-line x)
 (display x)
 (newline))

(define (divisible? x y) (= (remainder x y) 0))

;; Useful stream utility functions

(define (stream-filter pred stream)
  (cond ((empty-stream? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;;  Mapping functions

(define (stream-map proc stream)
  (if (empty-stream? stream)
      the-empty-stream
      (cons-stream (proc (stream-car stream))
                   (stream-map proc (stream-cdr stream)))))

;;  Iterating along a stream.

(define (stream-for-each proc stream)
  (if (empty-stream? stream)
      'done
      (begin (proc (stream-car stream))
             (stream-for-each proc (stream-cdr stream)))))

;;  Streams of numbers

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else (cons-stream (+ (stream-car s1) (stream-car s2))
                           (add-streams (stream-cdr s1) (stream-cdr s2))))))

(define (scale-stream c stream)
  (stream-map (lambda (x) (* x c)) stream))

;;  Differs from book by not checking for empty streams
(define (interleave s1 s2)
  (cons-stream (stream-car s1)
               (interleave s2
                           (stream-cdr s1))))

(define (merge s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (let ((h1 (stream-car s1))
               (h2 (stream-car s2)))
           (cond ((< h1 h2) (cons-stream h1 (merge (stream-cdr s1) s2)))
                 ((> h1 h2) (cons-stream h2 (merge s1 (stream-cdr s2))))
                 (else (cons-stream h1 
                                    (merge (stream-cdr s1) 
                                           (stream-cdr s2)))))))))

;; This next procedure is to be used in forming streams of pairs,
;; once you have defined the procedure MERGE-WEIGHTED
(define (weighted-pairs s t pair-weight)
  (cons-stream (cons (stream-car s) (stream-car t))
               (merge-weighted
                  (stream-map (lambda (x) (cons (stream-car s) x))
                              (stream-cdr t))
                  (weighted-pairs (stream-cdr s) (stream-cdr t) pair-weight)
                  (lambda (p) (pair-weight (car p) (cdr p))))))

;; This procedure forms streams of weighted pairs, where pairs of the
;; same weight have been combined.  In order to use it, you must
;; define an appropriate procedure COMBINE-SAME-WEIGHTS
(define (same-weight-pairs s t pair-weight)
  (combine-same-weights (weighted-pairs s t pair-weight)
                        pair-weight))

(define print-stream
  (let ()
    (define (iter s)
      (if (empty-stream? s)
          (display "]")
          (begin (write (stream-car s))
                 (write " ")
                 (iter (stream-cdr s)))))
    (lambda (s)
      (write "")
      (iter s))))
;; You may wonder why PRINT-STREAM has been written in such an obscure
;; way, when
;; (define (print-stream s)
;;   (write "[")
;;   (stream-for-each (lambda (x) (write x) (write " ")) s)
;;   (write "]"))
;; would have the same effect.  If you think about the "actor model"
;; and tail recursion, however, you may begin to see why.

;;  For exercise 3.43
(define (show x)
  (write-line x)
  x)

(define (nth-stream n s)
  (if (= n 0)
      (stream-car s)
      (nth-stream (- n 1) (stream-cdr s))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (enumerate-interval (1+ low) high))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;My Work

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Problem 1

(define (no-2-3-5 x)
  (cond ((divisible? x 2) #f)
        ((divisible? x 3) #f)
        ((divisible? x 5) #f)
        (else #t)))

(define my-stream (stream-filter no-2-3-5 integers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Problem 2

(define (not-disvisible-by-seven? a)
  (if (not (integer? (/ a 7))) #t #f))

(define (not-disvisible-by-three? a)
  (if (not (integer? (/ a 3))) #t #f))

(define seven-stream (stream-filter not-disvisible-by-seven? integers))
(define three-stream (stream-filter not-disvisible-by-three? integers))

(define interleaved-stream (interleave seven-stream three-stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Problem 3 

(define alt (cons-stream 0 (interleave integers alt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Problem 4

(define (interleave-pairs s t)
     (cons-stream (cons (stream-car s) (stream-car t))
                  (interleave
                     (stream-map (lambda (x) (cons (stream-car s) x))
                                 (stream-cdr t))
                     (interleave-pairs (stream-cdr s) (stream-cdr t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Problem 5

(define (divisible-2 x) (integer? (/ x 2)))
(define (not-divisible-2 x) (not (integer? (/ x 2))))

(define ev (stream-filter divisible-2 integers))
(define odd (stream-filter not-divisible-2 integers))

(define (merge-weighted s1 s2 weight)
  (let ((h1 (stream-car s1))
        (h2 (stream-car s2)))
    (cond ((<= (weight h1) (weight h2)) (cons-stream h1 (merge-weighted (stream-cdr s1) s2 weight)))
          ((> (weight h1) (weight h2)) (cons-stream h2 (merge-weighted s1 (stream-cdr s2) weight))))))

(define (weight pair)
  (+ (car pair)) (cdr pair))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Problem 6

;Part A
(define (weight-i+j s)
  (+ (car s)) (cdr s))

(define ordered-i+j (merge-weighted
                     (interleave-pairs integers integers)
                     (interleave-pairs integers integers) weight-i+j))


;Part B
(define (cube x) (* x x x))

(define (weight-icubed+jcubed s)
  (+ (cube (car s)) (cube (cdr s))))

(define ordered-icubed+jcubed (merge-weighted
                               (interleave-pairs integers integers)
                               (interleave-pairs integers integers)
                               weight-icubed+jcubed))

;Part C
(define (special-weight pair)
  (+ (* 2 (car pair)) (* 3 (cdr pair)) (* 5 (car pair) (cdr pair))))

(define ordered-divisors (merge-weighted
                          (interleave-pairs (stream-filter no-2-3-5 integers) (stream-filter no-2-3-5 integers))
                          (interleave-pairs (stream-filter no-2-3-5 integers) (stream-filter no-2-3-5 integers))
                          special-weight))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Problem 7

;(define (pair-weight a b) )

;(define (combine-same-weights stream pair-weight))

