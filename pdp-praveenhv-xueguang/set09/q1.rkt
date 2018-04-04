#lang racket
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)
(require "throbber.rkt")
(require "clock.rkt")
(require "politician.rkt")
(require "interface.rkt")
(provide run
         make-metatoy
         make-throbber
         make-clock
         make-politician
         Metatoy<%>
         Toy<%>)
;===============================================================================
;; DATA DEFINITIONS

;; a ListOfX is one of:
;; -- empty
;; (cons X ListOfX)
;; TEMPLATE:
;; lox-fn: LOX -> ??
#;
(define (lox-fn lox)
  (if (empty? lox) ...
      (... (first lox) (rest lox))))

;===============================================================================
;; CONTANTS

(define NEW-THROBBER-EVENT "t")
(define NEW-CLOCK-EVENT "c")
(define NEW-POLITICIAN-EVENT "p")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MetaToy% Class

;; Constructor template for MetaToy%:
;;   (new MetaToy% [toys ListOfToys])
;; Interpretation: An object of class MetaToy% takes signals from
;; big-bang and distributes them to its objects as appropriate.

(define MetaToy%
  (class* object% (Metatoy<%>)
    ;===========================================================================
    (init-field toys) ;; ListOfToys
    ;===========================================================================
    (super-new)
    ;===========================================================================
    ;; get-toys : -> ListOfToy
    ;; RETURNS: the list of toy this metatoy has
    ;; Example: (send (new Metatoy% [toys lot])) -> lot
    ;; Strategy: return a field value of this object
    (define/public (get-toys) toys)
    ;===========================================================================
    ;; after-tick : -> World
    ;; RETURNS: this metatoy after a tick update
    ;; EXAMPLE: see test cases for example
    ;; Strategy: Use HOF map on toys
    (define/public (after-tick)
      (make-metatoy
        (map
         ;; Toy -> Toy
         ;; RETURNS: the toy after a tick update
          (lambda (toy) (send toy after-tick))
          toys)))
    ;===========================================================================
    ;; to-scene : -> Scene
    ;; RETURNS: a scene that depicts this metatoy
    ;; EXAMPLE: see test cases for example
    ;; Strategy: Use HOF foldr on toys
    (define/public (to-scene)
      (foldr
       ;; Toy Scene -> Scene
       ;; RETURNS: the given scene with the toy painted on it
        (lambda (toy scene)
          (send toy add-to-scene scene))
        EMPTY-CANVAS
        toys))
    ;===========================================================================
    ;; after-key-event : KeyEvent -> MetaToy
    ;; RETURNS: the state of this metatoy after given keyevent
    ;; EXAMPLE: see test cases for example
    ;; STRATEGY: Cases on kev
    ;; NOTE: "t", "c" and "p" create new throbber, clock and politician;
    ;;       other keystrokes are passed on to the toys in the metatoy.
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-THROBBER-EVENT)
         (make-metatoy (cons (new Throbber%) toys))]
        [(key=? kev NEW-CLOCK-EVENT)
         (make-metatoy (cons (new Clock%) toys))]
        [(key=? kev NEW-POLITICIAN-EVENT)
         (make-metatoy (cons (new Politician%) toys))]
        [else
          (make-metatoy
            (map
              (lambda (toy) (send toy after-key-event kev))
              toys))]))
    ;===========================================================================
    ;; after-mouse-event: Integer Integer MouseEvent-> Metatoy
    ;; GIVEN: a mouse location, a mouse event
    ;; RETURNS: the state of the metatoy that should follow the
    ;;          given mouse event at the given location.
    ;; EXAMPLE: see test cases for example
    ;; Strategy: case on mev
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (metatoy-after-button-down mx my)]
        [(mouse=? mev "drag")
         (metatoy-after-drag mx my)]
        [(mouse=? mev "button-up")
         (metatoy-after-button-up mx my)]
        [(mouse=? mev "move")
         (metatoy-after-move mx my)]
        [else this]))
    ;===========================================================================
    ;; the next few functions are local functions, not in the interface.
    ;===========================================================================
    ;; metatoy-after-button-down: Int Int -> Metatoy
    ;; RETURNS: this metatoy after button down event at given location
    ;; EXAMPLE:
    ;;   (send (new Politician%) metatoy-after-button-down mx my)
    ;;     -> (send (new Politician%) after-mouse-event mx my "button-down")
    ;; Strategy: use HOF map on toys
    (define (metatoy-after-button-down mx my)
      (make-metatoy
        (map
          (lambda (toy) (send toy after-button-down mx my))
          toys)))
    ;===========================================================================
    ;; metatoy-after-button-up Int Int -> Metatoy
    ;; RETURNS: this metatoy after button up event at given location
    ;; EXAMPLE:
    ;;   (send (new Politician%) metatoy-after-button-up mx my)
    ;;     -> (send (new Politician%) after-mouse-event mx my "button-up")
    ;; Strategy: use HOF map on toys
    (define (metatoy-after-button-up mx my)
      (make-metatoy
        (map
          (lambda (toy) (send toy after-button-up mx my))
          toys)))
    ;===========================================================================
    ;; metatoy-after-drag Int Int -> Metatoy
    ;; RETURNS: this metatoy after button drag event at given location
    ;; EXAMPLE:
    ;;   (send (new Politician%) metatoy-after-drag mx my)
    ;;     -> (send (new Politician%) after-mouse-event mx my "drag")
    ;; Strategy: use HOF map on toys
    (define (metatoy-after-drag mx my)
      (make-metatoy
        (map
          (lambda (toy) (send toy after-drag mx my))
          toys)))
    ;===========================================================================
    ;; metatoy-after-move Int Int -> Metatoy
    ;; RETURNS: this metatoy after move event at given location
    ;; EXAMPLE:
    ;;   (send (new Politician%) metatoy-after-move mx my)
    ;;     -> (send (new Politician%) after-mouse-event mx my "move")
    ;; Strategy: use HOF map on toys
    (define (metatoy-after-move mx my)
      (make-metatoy
       (map
        (lambda (toy) (send toy after-move mx my))
        toys)))
    ))
;; End of Metatoy Class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DELIVERABLE FUNCTIONS

;===============================================================================
;; run : PosNum -> Metatoy
;; GIVEN: a frame rate (in seconds/tick)
;; EFFECT: creates a MetaToy with no toys in it, and runs it using big-bang
;;         at the given frame rate.  Returns the final state of the Metatoy.
;; Strategy: combine simpler functions
(define (run rate)
  (big-bang (make-metatoy empty)
    (on-tick
      (lambda (m) (send m after-tick))
      rate)
    (on-draw
      (lambda (m) (send m to-scene)))
    (on-key
      (lambda (m kev)
        (send m after-key-event kev)))
    (on-mouse
      (lambda (m mx my mev)
        (send m after-mouse-event mx my mev)))))
;===============================================================================
;; make-metatoy : ListOfToys -> Metatoy
;; RETURNS: a Metatoy with the given list of toys.
;; NOTE: The Metatoy<%> interface extends the World<%> interface, so the
;;       result of make-metatoy is something that big-bang can use as a world.
;; Strategy: use constructor template for Metatoy
(define (make-metatoy toys) (new MetaToy% [toys toys]))
;===============================================================================
;; make-throbber: PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a throbber at the given position.
;; Strategy: use constructor template for Throbber
(define (make-throbber x y) (new Throbber% [x x] [y y]))
;===============================================================================
;; make-clock : PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a clock at the given position.
;; Strategy: use constructor template for Clock
(define (make-clock x y) (new Clock% [x x] [y y]))
;===============================================================================
;; make-politician : PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a politician at the given position.
;; Strategy: use constructor template for Politician
(define (make-politician x y) (new Politician% [x x] [y y]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(begin-for-test
  (local
    ((define m0 (make-metatoy (list (make-politician INIT-X INIT-Y)
                                    (make-throbber INIT-X INIT-Y)
                                    (make-clock INIT-X INIT-Y))))
     (define m1 (make-metatoy empty))
     (define m2 (make-metatoy (list (make-throbber INIT-X INIT-Y))))
     (define m3 (send m2 after-tick))
     (define m4 (make-metatoy (list (make-throbber INIT-X INIT-Y)
                                    (make-throbber INIT-X INIT-Y))))
     (define m5 (send (send m1 after-key-event NEW-THROBBER-EVENT)
                      after-key-event NEW-THROBBER-EVENT))
     (define m6 (send (send m2 after-key-event NEW-CLOCK-EVENT)
                      after-key-event NEW-POLITICIAN-EVENT))
     )
    (check-equal?
     (length (send m0 get-toys)) 3
     "there should be 3 toys in m0")
    (check-equal?
     (send (first (send m3 get-toys)) toy-data) 6
     "after one tick, the throbber radius should be 5+1-6")
    (check-equal?
     (send m2 to-scene) (send (send m2 after-mouse-event 0 0 "button-up") to-scene)
     "after button-up event, m2 should be same in appearance")
    (check-equal?
     (send m2 to-scene) (send (send m2 after-mouse-event 0 0 "button-down") to-scene)
     "after button-down outside throbber, m2 should be same in appearance")
    (check-equal?
     (send m2 to-scene) (send (send m2 after-mouse-event 0 0 "drag") to-scene)
     "after drag outside throbber, m2 should be same in appearance")
    (check-equal?
     (send m2 to-scene) (send (send m2 after-mouse-event 0 0 "move") to-scene)
     "after move event outside throbber, m2 should be same in appearance")
    (check-equal?
     (send m2 to-scene) (send (send m2 after-mouse-event 0 0 "enter") to-scene)
     "after irelavent mouse event, m2 should be same in appearance")
    (check-equal?
     (send m5 to-scene) (send m4 to-scene)
     "m4 and m5 should be identical in appearance")
    (check-equal?
     (send m6 to-scene) (send m0 to-scene)
     "m6 and m0 should be identical in appearance")
    (check-equal?
     (send m6 to-scene) (send (send m6 after-key-event "s") to-scene)
     "metatoy should not change after irrelavent key event")
  ))
