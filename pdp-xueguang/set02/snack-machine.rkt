;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snack-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "snack-machine.rkt")
(provide initial-machine
         machine-next-state
         machine-output
         machine-remaining-kale
         machine-remaining-carrots
         machine-bank
)
;; ==============================================================================

;; A CustomerInput is one of
;; -- a PosInt        interp: insert the specified number of quarters
;; -- "kale"          interp: request a bag of kale chips
;; -- "carrots"       interp: request a bag of carrots
;; -- "change"        interp: return all the unspent money that the
;;                              customer has inserted

;; ==============================================================================

;; A MachineOutput is one of
;; -- "kale"           interp: machine dispenses a bag of kale chips
;; -- "carrots"        interp: machine dispenses a bag of carrot sticks
;; -- "Out of Item"    interp: machine displays "Out of Item"
;; -- a PosInt         interp: machine releases the specified number of quarters
;; -- "Nothing"        interp: the machine does nothing
 
;; ==============================================================================

(define-struct machine-state (kale carrots bank change))
;; A MachineState is:
;; -- (make-machine-state NonNegInt NonNegInt NonNegInt NonNegInt)
;; Interp: the NonNegInt values represent:
;; KaleCount, CarrotsCount, BankBalance and ChangeBalance
;; both BankBalance and ChangeBalance are in quaters in stead of cents
;; Example:
;; (make-machine-state 5 5 0 0) ;; an machine with 5 kales, 5 carrots, no money
;; (make-machine-state 0 0 0 0) ;; an machine with nothing

;; Observer Template:
;; machine-state-fn : MachineState -> ??
;; (define (machine-state-fn ms)
;;   ...
;;   (machine-state-kale ms)
;;   (machine-state-carrots ms)
;;   (machine-state-bank ms)
;;   (machine-state-change ms)
;;   ...
;; )

;; ==============================================================================

;; initial-machine : NonNegInt NonNegInt -> MachineState
;; GIVEN: a number of bags of kale chips and carrot sticks
;; RETURNS: the state of a machine loaded with the given numbers of bags
;; of kale chips and carrot sticks, with an empty bank.
;; Design Strategy: use construction template of MachineState
(define (initial-machine kale carrots) (make-machine-state kale carrots 0 0))
;; examples:
;; (initial-machine 0 0) => (make-machine-state 0 0 0 0)
;; (initial-machine 10 10) => (make-machine-state 10 10 0 0)

;; ==============================================================================

;; insert-change : MachineState NonNegInt -> MachineState
;; GIVEN: a MachineState and number of changes (in quaters) added to the machine
;; RETURNS: MachineState with the change already added to it
(define (insert-change ms change)
  (make-machine-state
   (machine-state-kale ms) (machine-state-carrots ms)
   (machine-state-bank ms) (+ (machine-state-change ms) change)))
;; example:
;; (insert-change (initial-machine 0 0) 2) => (make-machine-state 0 0 0 2)

;; ==============================================================================

;; dispense-kale : MachineState -> MachineState
;; RETURNS: the MachineState after dispense of a kale
(define (dispense-kale ms)
  (make-machine-state
   (- (machine-state-kale ms) 1) (machine-state-carrots ms)
   (+ (machine-state-bank ms) 3) (- (machine-state-change ms) 3)))
;; example:
;; (dispense-kale (make-machine-state 2 2 0 2)) => (make-machine-state 1 2 2 0)


;; ==============================================================================

;; dispense-carrots : MachineState -> MachineState
;; RETURNS: the MachineState after dispense of a kale
(define (dispense-carrots ms)
  (make-machine-state
   (machine-state-kale ms) (- (machine-state-carrots ms) 1)
   (+ (machine-state-bank ms) 2) (- (machine-state-change ms) 2)))
;; example:
;; (dispense-carrots (make-machine-state 3 3 3 3)) => (make-machine-state 3 2 5 1)

;; ==============================================================================

;; machine-next-state : MachineState CustomerInput -> MachineState
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine that should follow the customer's input
;; Design Strategy: case on CustomerInput + template of MachineState
(define (machine-next-state ms cin)
  (cond [(integer? cin) (insert-change ms cin)]
        [(and (string=? cin "kale")
              (> (machine-state-kale ms) 0)
              (>= (machine-state-change ms) 3) ) (dispense-kale ms)]
        [(and (string=? cin "carrots")
              (> (machine-state-carrots ms) 0)
              (>= (machine-state-change ms) 2)) (dispense-carrots ms)]
        [(string=? cin "change")
              (make-machine-state (machine-state-kale ms)
                                  (machine-state-carrots ms)
                                  (machine-state-bank ms)
                                  0 )]
        [else ms]
  )
)

;; examples:
;; (machine-next-state (initial-machine 10 10) 10)
;;   => (make-machine-state 10 10 0 10)
;; (machine-next-state (machine-next-state (initial-machine 10 10) 10) "carrots")
;;   => (make-machine-state 10 9 2 8)
;; (machine-next-state (machine-next-state (initial-machine 10 10) 10) "kale")
;;   => (make-machine-state 9 10 3 7)
;; (machine-next-state (make-machine-state 10 10 0 2) "kale")
;;   => (make-machine-state 10 10 0 2)
;; (machine-next-state (make-machine-state 10 10 0 2) "change")
;;   => (make-machine-state 10 10 0 0)

;; ==============================================================================

;; machine-output : MachineState CustomerInput -> MachineOutput
;; GIVEN: a machine state and a customer input
;; RETURNS: a MachineOutput that describes the machine's response to the
;;          customer input
;; Design Strategy: case on CustomerInput + template of MachineState
(define (machine-output ms cin)
  (cond
    [(integer? cin) "Nothing"]
    [(string=? cin "kale")
              (if (= (machine-state-kale ms) 0) "Out of Item"
                  (if (>= (machine-state-change ms) 3) "kale" "Nothing"))]
    [(string=? cin "carrots")
              (if (= (machine-state-carrots ms) 0) "Out of Item"
                  (if (>= (machine-state-change ms) 2) "carrots" "Nothing"))]
    [else "Nothing"]
  )
)
;; examples:
;; (machine-output (make-machine-state 10 0 10 10) "carrots") => "Out of Item"
;; (machine-output (make-machine-state 0 10 10 10) "kale") => "Out of Item"
;; (machine-output (make-machine-state 0 0 0 1) "change") => "Nothing"
;; (machine-output (make-machine-state 0 0 0 0) 10) => "Nothing"
;; (machine-output (make-machine-state 10 10 10 10) "kale") => "kale"
;; (machine-output (make-machine-state 10 10 10 10) "carrots") => "carrots"
;; (machine-output (make-machine-state 10 10 10 0) "kale") => "Nothing"
;; (machine-output (make-machine-state 10 10 10 0) "carrots") => "Nothing"

;; ==============================================================================

;; machine-remaining-kale : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of bags of kale chips left in the machine
(define (machine-remaining-kale ms) (machine-state-kale ms))
;; example:
;; (machine-remaining-kale (initial-machine 10 3)) => 10

;; ==============================================================================

;; machine-remaining-carrots : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of bags of carrots left in the machine
(define (machine-remaining-carrots ms) (machine-state-carrots ms))
;; example:
;; (machine-remaining-carrots (initial-machine 4 7)) => 7

;; ==============================================================================
 
;; machine-bank : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the amount of money in the machine's bank, in cents
(define (machine-bank ms) (* (machine-state-bank ms) 25))
;; example:
;; (machine-bank (make-machine-state 10 10 10 10)) => 250


;; ==============================================================================

(begin-for-test
  (check-equal? (initial-machine 0 0) (make-machine-state 0 0 0 0))
  (check-equal? (initial-machine 10 10) (make-machine-state 10 10 0 0))
  (check-equal? (dispense-kale (make-machine-state 2 2 0 3))
                (make-machine-state 1 2 3 0))
  (check-equal? (insert-change (initial-machine 0 0) 2)
                (make-machine-state 0 0 0 2))
  (check-equal? (dispense-carrots (make-machine-state 3 3 3 3))
                (make-machine-state 3 2 5 1))
  (check-equal? (machine-next-state (initial-machine 10 10) 10)
                (make-machine-state 10 10 0 10))
  (check-equal? (machine-next-state (machine-next-state
                                     (initial-machine 10 10) 10) "kale")
                (make-machine-state 9 10 3 7))
  (check-equal? (machine-next-state (machine-next-state
                                     (initial-machine 10 10) 10) "carrots")
                (make-machine-state 10 9 2 8))
  (check-equal? (machine-next-state (make-machine-state 10 10 0 2) "kale")
                (make-machine-state 10 10 0 2))
  (check-equal? (machine-next-state (make-machine-state 10 10 0 2) "change")
                (make-machine-state 10 10 0 0))
  (check-equal? (machine-bank (make-machine-state 10 10 10 10)) 250)
  (check-equal? (machine-remaining-carrots (initial-machine 4 7)) 7)
  (check-equal? (machine-remaining-kale (initial-machine 10 3)) 10)
  (check-equal? (machine-output (make-machine-state 10 0 10 10) "carrots")
                "Out of Item")
  (check-equal? (machine-output (make-machine-state 0 0 0 1) "change")
                "Nothing")
  (check-equal? (machine-output (make-machine-state 0 0 0 0) 10)
                "Nothing")
  (check-equal? (machine-output (make-machine-state 0 10 10 10) "kale")
                "Out of Item")
  (check-equal? (machine-output (make-machine-state 10 10 10 10) "kale")
                "kale")
  (check-equal? (machine-output (make-machine-state 10 10 10 10) "carrots")
                "carrots")
  (check-equal? (machine-output (make-machine-state 10 10 10 0) "kale")
                "Nothing")
  (check-equal? (machine-output (make-machine-state 10 10 10 0) "carrots")
                "Nothing")
)