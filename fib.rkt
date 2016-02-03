(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


