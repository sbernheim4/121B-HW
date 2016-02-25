;; This is the code for ``Computer Psychiatrist'' (Doctor)

; for Racket users...
(#%require (only racket/base random))
; *******************

; I added the parameter max-number-patients 
(define (visit-doctor name max-num-patients)
  (write-line (list 'hello name))
  (write-line '(what seems to be the trouble?))
  (doctor-driver-loop name '() max-num-patients))

; A procedure which will determine if the loop should continue or if the doctor is done for the day based on
; if the docotor has seen all the patients he can.
(define (auto-start num-patients)
  (cond ((= num-patients 0)
         (write-line '(done for the day)))
        (else (continue num-patients))))

; This procedure will continue the doctors routine by asking the new patient their name and then calling
; visit doctor 
(define (continue num-patients)
  (write-line '(WHO ARE YOU?))
   (let ((name (read)))
     (visit-doctor (car name) num-patients)))

; I added the lst and max-number-patients parameters to the loop to keep track of this info. 
(define (doctor-driver-loop name lst max-num-patients)
  (display max-num-patients)
  (newline)
  (write '**)
  (let ((user-response (read)))
    (cond ((equal? user-response '(goodbye)) ; if the user enters goodbye
             (write-line (list 'goodbye name))
             (write-line '(see you next week))
             (write-line '(NEXT!))
             (auto-start (- max-num-patients 1)))
          ((equal? user-response '(suppertime)) ; if the response of a user is suppertime
             (write-line '(time to go home)))
          (else (write-line (reply user-response lst)) ; else call the doctor driver loop and continue seeing the current patient
             (doctor-driver-loop name (cons lst user-response))))))

; reply now has three options based on if the random number is a 0, 1 or 2
(define (reply user-response lst)
  (let ((x (random 3)))
  (cond ((eq? x 0)
           (append (qualifier)
                   (change-person user-response)))
        ((and (eq? x 1) (not (null? lst)))
         ;             beginning phrase        randomly chooses a previous response 
           (append '(earlier you said that) (index-chooser lst (random-index lst))))
        (else (hedge)))))

; this procedure will chooses an element from a list given an index
(define (index-chooser lst index)
  (if (eq? index 0)
       (cdr lst)
       (index-chooser (car lst) (- index 1))))

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

; NEW replace PROCEDURE
(define (replace target replacement-pairs)
  (cond ((null? replacement-pairs)
        target)
        ((eq? (car (car replacement-pairs)) target) (cadr (car replacement-pairs)))
        (else (replace target (cdr replacement-pairs)))))

; NEW many-replace PROCEDURE
(define (many-replace replacement-pairs sentence)
  (cond ((null? replacement-pairs)
         sentence)
        ((null? sentence) sentence)
        (else (cons (replace (car sentence) replacement-pairs)
                    (many-replace replacement-pairs (cdr sentence))))))

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