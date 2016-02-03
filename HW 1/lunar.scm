;;; this is the code for problem set -- Lunar Lander

(define (update ship-state fuel-burn-rate)
  (make-ship-state
   (+ (height ship-state) (* (velocity ship-state) dt)) ; height

   (if (= (fuel ship-state) 0)
       (- (velocity ship-state) (* gravity dt))
       (+ (velocity ship-state) (* (- (* engine-strength fuel-burn-rate) gravity) dt))) ; velocity
   
   (if (>= (* fuel-burn-rate dt) (fuel ship-state))
       0
       (- (fuel ship-state) (* fuel-burn-rate dt)))))   ; fuel

; I added a second parameter called choice which would represent something like
; full-burn or ask-user. I would then test to see if choice was equal to the
; full-burn procedure in which case the get-fuel-burn-rate was replaced by a 1.
; If choice was ask-user then the else clause would be executed.
(define (lander-loop ship-state choice)
  (show-ship-state ship-state)
  (if (landed? ship-state) (end-game ship-state)
      (lander-loop (update ship-state (choice ship-state)) choice)))

(define (show-ship-state ship-state)
  (write-line 
    (list 'height (height ship-state)
          'velocity (velocity ship-state)
          'fuel (fuel ship-state))))

(define (landed? ship-state)
  (<= (height ship-state) 0))

(define (end-game ship-state)
  (let ((final-velocity (velocity ship-state)))
       (write-line final-velocity)
       (cond ((>= final-velocity safe-velocity)
               (write-line "good landing")
               'game-over)
             (else
               (write-line "you crashed!")
               'game-over))))

(define (get-burn-rate)
  (if (= (player-input) burn-key)
      1
      0))

; play now takes another parameter which is a strategy
(define (play choice)
  (lander-loop (initial-ship-state) choice)
)

(define (initial-ship-state)
  (make-ship-state 50       ; 50 km high
                   0        ; not moving (0 km/sec)
                   20))     ; 20 kg of fuel left

(define dt .1)               ; 1 second interval of simulation
  
(define gravity 0.5)        ; 0.5 km/sec/sec
  
(define safe-velocity -0.5) ; 0.5 km/sec or faster is a crash

(define engine-strength 1)  ; 1 kilonewton-second

(define (player-input) 
  (char->integer (prompt-for-command-char " action: "))) 

(define burn-key 32)   ;space key

; You'll learn about the stuff below here in Chapter 2.
; For now, think of make-ship-state, height, velocity, and fuel
; as primitive procedures built in to Scheme.

(define (make-ship-state height velocity fuel)
  (list 'HEIGHT   height
        'VELOCITY velocity
        'FUEL     fuel))

(define (height state) (second state))
(define (velocity state) (fourth state))
(define (fuel state) (sixth state))

(define (second l) (cadr l))
(define (fourth l) (cadr (cddr l)))
(define (sixth l) (cadr (cddr (cddr l))))

; Users of DrScheme or DrRacket: add these for compatibility with MIT Scheme...

; for input and output

(define (write-line x)
  (display x)
  (newline))

(define (get-one-key)
  (let ((x (read-char)))
    (if (eq? x #\newline)
        x
        (empty-buffer x))))

(define (empty-buffer x)
  (if (eq? (read-char) #\newline)
      x
      (empty-buffer x)))

(define (prompt-for-command-char prompt)
  (display prompt)
  (get-one-key)) 

; for random number generation

(#%require (only racket/base random))

; a ridiculous addendum  (you'll need this for the exercises)

(define (1+ x) (+ 1 x))

; My Additional Work

(define (full-burn ship-state) 1)
(define (no-burn ship-state) 0)
(define (ask-user ship-state) (get-burn-rate))

;(define (random-choice a b)
 ;   (if (= (random 2) 0)
  ;      a
   ;     b))

;(define (height-choice a b bounded-height)
 ; (lambda (ship-state)
  ;  (if (> (height ship-state) bounded-height)
   ;     (a ship-state)
    ;    (b ship-state))))

(define (height-choice strategy-1 strategy-2 bounded-height)
  (choice strategy-1
          strategy-2
          (lambda (ship-state) (>= (height ship-state) bounded-height))))

(define (random-choice strategy-1 strategy-2)
  (choice strategy-1
          strategy-2
          (lambda (ship-state) (= (random 2) 0))))

(define (choice a b pred)
  (lambda (ship-state) 
  (if (pred ship-state)
      (a ship-state)
      (b ship-state))))


(define (constant-acc ship-state)
    (/ (+ (/ (* (velocity ship-state) (velocity ship-state)) (* 2 (height ship-state))) gravity) engine-strength))