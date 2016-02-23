;; This is the code for ``Computer Psychiatrist'' (Doctor)

; for Racket users...
(#%require (only racket/base random))
; *******************

(define (visit-doctor name)
  (write-line (list 'hello name))
  (write-line '(what seems to be the trouble?))
  (doctor-driver-loop name '()))

(define (doctor-driver-loop name lst)
  (newline)
  (write '**)
  (let ((user-response (read)))
    (cond ((equal? user-response '(goodbye))
             (write-line (list 'goodbye name))
             (write-line '(see you next week)))
          (else (write-line (reply user-response lst))
                (doctor-driver-loop name (cons lst user-response))))))

(define (reply user-response lst)
  (let ((x (random 3)))
    (display x)
  (cond ((eq? x 0)
           (append (qualifier)
                   (change-person user-response)))
        ((eq? x 1)
           (append '(earlier you said that) (random-chooser lst (random-index lst))))
        (else (hedge)))))

; this procedure will chooses an element from a list given an index
(define (random-chooser lst index)
  (if (eq? index 0)
       (cdr lst)
       (random-chooser (car lst) (- index 1))))

; this procedure will choose a random number between 0 and the size of a given list 
(define (random-index lst)
      (random (size lst)))

; This procedure will get the size of a list 
(define (size l)
  (define (size-iter l count)
    (if (null? l)
        count
        (size-iter (cdr l) (+ 1 count))))
  (size-iter l 0)
)

(define (fifty-fifty)
  (= (random 2) 0))


(define (qualifier)
  (pick-random '((you seem to think)
                 (you feel that)
                 (why do you believe)
                 (why do you say)
                 (how is this negative)
                 (what makes you feel bad)
                 (where does this come from)
                 )))

(define (hedge)
  (pick-random '((please go on)
                 (many people have the same sorts of feelings)
                 (many of my patients have told me the same thing)
                 (please continue)
                 (can you tell me more about your childhood)
                 (this is interesting, can you expand)
                 (I know many people with similar issues. maybe they could help you too))))

(define (replace pattern replacement lst)
  (cond ((null? lst) '())
        ((equal? (car lst) pattern)
           (cons replacement
                 (replace pattern replacement (cdr lst))))
        (else (cons (car lst)
              (replace pattern replacement (cdr lst))))))

(define (many-replace replacement-pairs lst)
  (cond ((null? replacement-pairs) lst)
         (else (let ((pat-rep (car replacement-pairs)))
            (replace (car pat-rep)
                     (cadr pat-rep)
                     (many-replace (cdr replacement-pairs)
                     lst))))))

(define (change-person phrase)
  (many-replace '((me you) (are am) (you i) (your my) (i you) (am are) (my your))
                phrase))

(define (pick-random lst)
  (nth (+ 1 (random (length lst))) lst))

;;******

(define (prob n1 n2)
  (< (random n2) n1))

(define (ask-patient-name)
  (write-line '(next!))
  (write-line '(who are you?))
  (car (read)))

(define (nth n lst)
  (if (= n 1) 
      (car lst)
      (nth (- n 1) (cdr lst))))
;;******

(define (atom? a) (not (pair? a)))

(define nil '())

(define (write-line x) (begin (write x) (newline)))

;;******

;; DELETEABLE WORK HERE
(define x '((me you) (are am) (you i) (your my) (i you) (am are) (my your)))
