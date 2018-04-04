;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "fsm.rkt")

(provide
  initial-state
  next-state
  accepting-state?
  error-state?
  ) 

;; =======================================================================

;; A MachineInput is a 1string which can be: "q", "x", "u", "a", "b", "d", "e", and "f"

;; =======================================================================

;; A MachineInputList is a list containing all the MachineInput in squence
(define machine-input-list (list "q" "x" "u" "a" "b" "d" "e" "f"))

;; =======================================================================

(define-struct state (accepting-inputs))
;; A State can be:
;; -- (make-state machine-input-list)
;;    interp: the initial state, accepts all legal inputs, qualified as accepting state
;;            triggered by initial-state function or by input "u"
;;
;; -- (make-state (list "u" "a" "b" "d" "e" "f"))
;;    interp: accepts "u","a","b","d","e","f" as input, qualified as accepting state
;;            triggered by input "u"
;;
;; -- (make-state (list "a" "b" "d" "e" "f"))
;;    interp: accepts "a","b","d","e","f" as input, qualified as accepting state
;;            triggered by input "a" or "b"
;;
;; -- (make-state (list "e" "f"))
;;    interp: accepts "e","f" as input, qualified as accepting state
;;            triggered by input "d", "e" or "f"
;;
;; -- (make-state (list))
;;    interp: accepts no input, not qualified as accepting state
;;            triggered by illegal inputs
;;
;; Observer Template:
;; state-fn : State -> ??
;; (define (state-fn s)
;;   ...
;;   (state-acceting-inputs s)
;; )

;; =======================================================================

;; initial-state : Number -> State
;; GIVEN: a number
;; RETURNS: a representation of the initial state
;; of your machine.  The given number is ignored.
(define (initial-state number) (make-state machine-input-list))
;; example:
;; (initial-state 10) => (make-state machine-input-list)
;; (initial-state 0) => (make-state machine-input-list)

;; =======================================================================

;; input-legal? : State input -> boolean
;; GIVEN: a State and a MachineInput
;; RETURNS: whether the MachineInput is legal input for the given State
(define (input-legal? s input) (member input (state-accepting-inputs s)))
;; example:
;; (input-legal? (make-state (list "e" "f")) "q") => false
;; (input-legal? (make-state machine-input-list) "q") => true

;; =======================================================================

;; next-state : State MachineInput -> State
;; GIVEN: a state of the machine and a machine input
;; RETURNS: the state that should follow the given input.
;; Design Strategy: Case on MachineInput
(define (next-state s input)
  (if
   (input-legal? s input)
      (cond
        [(member input (list "q" "x"))     (initial-state 0)]
        [(member input (list "u"))         (make-state (list "u" "a" "b" "d" "e" "f"))]
        [(member input (list "a" "b"))     (make-state (list "a" "b" "d" "e" "f"))]
        [(member input (list "d" "e" "f")) (make-state (list "e" "f"))]
        )
   (make-state (list))
  )
)
;; example:
;; (next-state (make-state machine-input-list) "e") => (make-state (list "e" "f"))
;; (next-state (make-state (list "e" "f")) "u") => (make-state (list))
;; (next-state (make-state (list "u" "a" "b" "d" "e" "f") "a") => (make-state (list "a" "b" "d" "e" "f"))

;; =======================================================================

;; accepting-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the given state is a final (accepting) state
;; Design Strategy: Combine simpler functions
(define (accepting-state? s) (> (length (state-accepting-inputs s)) 0))
;; examples:
;; (accepting-state? (make-state (list "e" "f"))) => true
;; (accepting-state? (make-state machine-input-list)) => true
;; (accepting-state? (make-state (list))) => false

;; =======================================================================

;; error-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff there is no path (empty or non-empty) from the given
;;          state to an accepting state
;; Design Strategy:
(define (error-state? s) (not (accepting-state? s)))
;; (error-state? (make-state (list "e" "f"))) => false
;; (error-state? (make-state machine-input-list)) => false
;; (error-state? (make-state (list))) => true

;; =======================================================================

(begin-for-test
  (check-equal? (initial-state 10) (make-state machine-input-list))
  (check-equal? (initial-state 0) (make-state machine-input-list))
  
  (check-equal? (input-legal? (make-state (list "e" "f")) "q") false)
  (check-equal? (input-legal? (make-state machine-input-list) "q") true)
  
  (check-equal? (next-state (make-state machine-input-list) "q") (make-state machine-input-list))
  (check-equal? (next-state (make-state machine-input-list) "u") (make-state (list "u" "a" "b" "d" "e" "f")))
  (check-equal? (next-state (make-state machine-input-list) "a") (make-state (list "a" "b" "d" "e" "f")))
  (check-equal? (next-state (make-state machine-input-list) "e") (make-state (list "e" "f")))
  (check-equal? (next-state (make-state machine-input-list) "f") (make-state (list "e" "f")))
  (check-equal? (next-state (make-state (list "u" "a" "b" "d" "e" "f")) "q") (make-state (list)))
  (check-equal? (next-state (make-state (list "e" "f")) "a") (make-state (list)))
  
  (check-equal? (accepting-state? (make-state (list "e" "f"))) true)
  (check-equal? (accepting-state? (make-state machine-input-list)) true)
  (check-equal? (accepting-state? (make-state (list))) false)
  
  (check-equal? (error-state? (make-state (list "e" "f"))) false)
  (check-equal? (error-state? (make-state machine-input-list)) false)
  (check-equal? (error-state? (make-state (list))) true)
)

