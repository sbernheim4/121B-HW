;; This is the code for -- Stable Marriage

(define (match-make proposers proposees)
  (send proposers 'reset)
  (send proposees 'reset)
  (courtship proposers proposers proposees)
  (zip-together (send proposers 'name)
                (send
                 (send proposers 'intended) 'name)))

(define (courtship unengaged-proposers proposers proposees) ... )

; Check to see if the person person1 likes is the same as person2 and if the person person2 likes is person 1
; if this is true return true. Otherwise return false. 
(define (couple? person1 person2)
  (let ((intended1 (person1 'intended)) (intended2 (person2 'intended)))
    (if (and (eq? intended1 person2) (eq? intended2 person1))
        #t
        #f)))

; given two people, and a third person C as a lambda variable, the person C likes more will be returned 
(define (i-like-more? person1 person2)
  (lambda (me)
  (let ((index1 (get-index person1 (me 'loves) 0)) (index2 (get-index person2 (me 'loves) 0)))
    (if (> index1 index2)
        person2
        person1))))

; given a list, and an element and an index to start from, the index of where element appears in list will be returned
(define (get-index element lst index)
  (cond ((null? lst) -1)
        ((eq? (car lst) element) (+ index 1))
        (else (get-index element (cdr lst) (+ index 1)))))

(define (currently-unengaged list-of-people)
  (lambda (unengaged-list)
          ;if everyone has been checked return unengaged list 
    (cond ((null? list-of-people) unengaged-list)
          ; if the first person is unengaged make the recursive call but first adding the first person to the lambda list parameter using set!
          ((null? ((car list-of-people) 'intended)) (begin (set! unengaged-list (append unengaged-list (car list-of-people))) ((currently-unengaged (cdr list-of-people)) unengaged-list)))
          ; otherwise make the recursive call on the cdr of the list-of-people
          (else ((currently-unengaged (cdr list-of-people)) unengaged-list)))))

(define (send list-of-people message)
  (if (not (null? list-of-people)) (begin (send (cdr list-of-people) message) ((car list-of-people) message))))

(define (zip-together list1 list2)
  (if (null? list1)
      '()
      (cons (list (car list1) (car list2))
            (zip-together (cdr list1) (cdr list2)))))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (make-person my-name)
  (let ((preference-list '())
        (possible-mates '())
        (current-intended '()))
    (define (me message)
      (cond ((eq? message 'name) my-name)
            ((eq? message 'intended) current-intended)
            ((eq? message 'loves) preference-list)
            ((eq? message 'possible) possible-mates)
            ((eq? message 'reset)
               (set! current-intended '())
               (set! possible-mates preference-list)
               'reset-done)
            ((eq? message 'load-preferences)
               (lambda (plist)
                  (set! preference-list plist)
                  (set! possible-mates plist)
                  (set! current-intended '())
                  'preferences-loaded))
            ((eq? message 'propose)
               (let ((beloved (car possible-mates)))
                 (set! possible-mates (cdr possible-mates))
                 (if (eq? ((beloved 'i-love-you) me)
                          'i-love-you-too)
                     (begin (set! current-intended beloved)
                            'we-are-engaged)
                     'no-one-loves-me)))
            ((eq? message 'i-love-you)
             (lambda (asker)
               ; if the proposer is unengaged then set the receiver's intended to asker and return i-love-you-too
               (cond ((null? current-intended) (begin (set! current-intended asker) 'i-love-you-too))
                     ; if the receiver is engaged, determine if the receiver likes their current intended or the proposer more and then either
                     ; change the engagement and return i-love-you-too or return buzz-off-creep 
                     ((eq? (i-like-more? current-intended asker) asker)
                          (begin ((car current-intended) 'i-changed-my-mind) (set! current-intended asker) 'i-love-you-too))
                     (else 'buzz-off-creep))))
            ((eq? message 'i-changed-my-mind)
               (lambda (lost-love)
                  (cond ((eq? current-intended lost-love)
                            (set! current-intended '())
                            'dumped!)
                        (else 
                            'there-must-be-some-misunderstanding))))
            (else 
              (error "Bad message to a person " (list me my-name message)))))
      me))

;; This is a test file for -- Stable Marriage

(define alan (make-person 'Alan))
(define bob (make-person 'Bob))
(define charles (make-person 'Chuck))
(define david (make-person 'Dave))
(define ernest (make-person 'Ernie))
(define franklin (make-person 'Frank))
(define agnes (make-person 'Agnes))
(define bertha (make-person 'Bertha))
(define carol (make-person 'Carol))
(define deborah (make-person 'Debbie))
(define ellen (make-person 'Ellen))
(define francine (make-person 'Fran))

((alan 'load-preferences) 
   (list agnes carol francine bertha deborah ellen))
((bob 'load-preferences) 
   (list carol francine bertha deborah agnes ellen))
((charles 'load-preferences) 
   (list agnes francine carol deborah bertha ellen))
((david 'load-preferences) 
   (list francine ellen deborah agnes carol bertha))
((ernest 'load-preferences) 
   (list ellen carol francine agnes deborah bertha))
((franklin 'load-preferences) 
   (list ellen carol francine bertha agnes deborah))
((agnes 'load-preferences) 
   (list charles alan bob david ernest franklin))
((bertha 'load-preferences) 
   (list charles alan bob david ernest franklin))
((carol 'load-preferences) 
   (list franklin charles bob alan ernest david))
((deborah 'load-preferences) 
   (list bob alan charles franklin david ernest))
((ellen 'load-preferences) 
   (list franklin charles bob alan ernest david))
((francine 'load-preferences) 
   (list alan bob charles david franklin ernest))

(define men (list alan bob charles david ernest franklin))
(define women (list agnes bertha carol deborah ellen francine))
