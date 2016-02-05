;Samuel Bernheim (bernheim@brandeis.edu) 2016-2-25

(define z (cons 1 (cons 3 (cons (cons 5 7) 9))))

(cdr (car (cdr (cdr z))))


