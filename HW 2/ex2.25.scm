;Samuel Bernheim (bernheim@brandeis.edu) 2016-2-25

(define x '(1 3 (5 7) 9))
(car (cdr (car (cdr (cdr x)))))

(define y (list (list 7)))
(car (car y))

(define z '(1 (2 (3 (4 (5 (6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z))))))))))))

