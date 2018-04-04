#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "interfaces.rkt")
(require "constants.rkt")
(require "throbber.rkt")
(require "clock.rkt")
(require "politician.rkt")

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

;;==============================================================================
;; MetaToy% Class

;; Constructor template for MetaToy%:
;;   (new MetaToy% [toys ListOfToys])
;; Interpretation: An object of class MetaToy% takes signals from
;; big-bang and distributes them to its objects as appropriate.
(define Metatoy%
  (class* object% (Metatoy<%>)
    
;===============================================================================    
;;ListOfToys
    (init-field toys) 
;===============================================================================    
    (super-new) 
;===============================================================================
     ;; after-tick : -> Void
    ;; GIVEN: No arguements
    ;; EFFECT: updates this Metatoy to the state it 
    ;; should have following a tick.
    ;; DETAILS: The change in all the stateful widgets in the
    ;; Metatoy after a tick is the change in the
    ;; Metatoy.
    ;; STRATEGY: Use HOF for-each on the toys (SWidget's) in this
    ;; Metatoy
    (define/public (after-tick)
      (begin
        (for-each
         ;; Stateful-Widget -> Void
         ;; EFFECT: updates this widget to the state it should have
         ;; following a tick.
         (lambda (toy) (send toy after-tick))
         toys)))
    
;===============================================================================
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: A Scene
    ;; RETURNS: A Scene like this one, but with all the widgets
    ;; placed on the canvas
    ;; STRATEGY: Use HOF foldr on the stateful widget's present in the Metatoy
    (define/public (add-to-scene scene)
      (foldr
       
       (lambda (toy scene)
         (send toy add-to-scene scene))
       scene
       toys))
    
;===============================================================================
    ;;after-key-event : Key -> Void
    ;;GIVEN: A key event
    ;;EFFECT: Updates the ListOfToys(toys) of the metatoy object. Adds new toys
    ;;        to the list of toys after the following key events. Result if the
    ;;        new state of the metatoy
    ;;Key events that modify the state.
    ;;"t" -> Adds a new Throbber.
    ;;"c" -> Adds a new clock.
    ;;"p" -> Adds a new politician.
    ;;Strategy : Cases on KeyEvent kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-THROBBER-EVENT)
         (set! toys (cons (make-throbber INIT-X INIT-Y) toys))]
        [(key=? kev  NEW-CLOCK-EVENT)
         (set! toys (cons (make-clock INIT-X INIT-Y) toys))]
        [(key=? kev  NEW-POLITICIAN-EVENT)
         (set! toys (cons (make-politician INIT-X INIT-Y) toys))]
        [else this]
        ))
;===============================================================================
    ;;after-mouse-event: PosInt PosInt Mouse-Event->Void
    ;;GIVEN:  The mouse coordinates and a mouse-event
    ;;EFFECT: Updates the Metatoy to the state it should follow after the mouse
    ;;        event. All the toys in the metatoy are updated as the event is
    ;;        forwarded to all the toys in the listoftoys.
    ;;Strategy : Cases on mouse event mev.
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (after-button-down mx my)]
        [(mouse=? mev "drag")
         (after-drag mx my)]
        [(mouse=? mev "button-up")
         (after-button-up mx my)]
        [(mouse=? mev "move")
         (after-move mx my)]
        [else this]))
;===============================================================================
    ;; get-toys : -> ListOfToy
    ;; RETURNS: the list of toy this metatoy has
    ;; Example: (send (new Metatoy% [toys lot])) -> lot
    ;; Strategy: return a field value of this object
    (define/public (get-toys)
      toys)
    
;===============================================================================    
    ;;after-button-down : PosInt PosInt-> void
    ;;GIVEN: Mouse co-ordinates in the x y plane
    ;;EFFECT: toys present in the list of toys of the metatoy are updated to the
    ;;        state that should follow the current state due to mouse event.
    ;;Strategy:  Use HOF for-each on the toys (SWidget's) in this
    ;;           Metatoy
    (define/public (after-button-down mx my)
      (begin 
      (for-each
        (lambda (toy) (send toy after-button-down mx my))
        toys)
       ))
;===============================================================================
    ;;after-button-up : PosInt PosInt-> void
    ;;EFFECT: toys present in the list of toys of the metatoy are updated to the
    ;;        state that should follow the current state due to mouse event.
    ;;Strategy:  Use HOF for-each on the toys (SWidget's) in this
    ;;           Metatoy
    (define/public (after-button-up mx my)
      (begin
       (for-each
        (lambda (toy) (send toy after-button-up mx my))
        toys)
       ))
;===============================================================================
    ;;after-drag : PosInt PosInt-> void
    ;;EFFECT: toys present in the list of toys of the metatoy are updated to the
    ;;        state that should follow the current state due to mouse event.
    ;;Strategy:  Use HOF for-each on the toys (SWidget's) in this
    ;;           Metatoy
    (define/public (after-drag mx my)
      (begin
       (for-each
        (lambda (toy) (send toy after-drag mx my))
        toys)
       ))
;===============================================================================
    ;;after-move : PosInt PosInt-> void
    ;;EFFECT: toys present in the list of toys of the metatoy are updated to the
    ;;        state that should follow the current state due to mouse event.
    ;;Strategy:  Use HOF for-each on the toys (SWidget's) in this
    ;;           Metatoy
    (define/public (after-move mx my)
      (begin
       (for-each
        (lambda (toy) (send toy after-move mx my))
        toys)))
    
    ))
;;=============================================================================
;;                    END OF CLASS METATOY
;;=============================================================================
;; make-metatoy : ListOfToys -> Metatoy
;; RETURNS: a Metatoy with the given list of toys.
;; NOTE: The Metatoy<%> interface extends the SWidget<%> interface, so the
;;       result of make-metatoy is something that big-bang can use as a world.
;; Strategy: use constructor template for Metatoy
(define (make-metatoy toys) (new Metatoy% [toys toys]))


;===============================================================================
;;make-throbber :  PosInt PosInt -> Toy
;;GIVEN: an x and a y position
;;RETURNS: an object representing a throbber at the given position.
;;Strategy: use constructor template for Throbber
(define (make-throbber x y) (new Throbber% [x x][y y] ))
;===============================================================================
;;make-clock : PosInt PosInt -> Toy
;;GIVEN: an x and a y position
;;RETURNS: an object representing a clock at the given position.
;;Strategy: use constructor template for Clock
(define (make-clock x y) (new Clock% [x x][y y] ))
;===============================================================================
;;make-politician : PosInt PosInt -> Toy
;;GIVEN: an x and a y position
;;RETURNS: an object representing a politician at the given position.
;;Strategy: use constructor template for Politician
(define (make-politician x y) (new Politician% [x x][y y] ))
;===============================================================================


;===============================================================================
;;run : PosInt->World.
;; GIVEN: a frame rate, in secs/tick.
;; EFFECT: runs this world at the given frame rate.
;; RETURNS: the world in its final state of the world.
(define (run rate)
   (local
     (
      (define cont-obj (container-init CANVAS-WIDTH CANVAS-HEIGHT))
       )
   (begin
     (send cont-obj add-stateful-widget (make-metatoy empty))
      (send cont-obj run rate))))
;===============================================================================
;;TEST CASES;
(begin-for-test
  (local
    ((define m0 (make-metatoy (list (make-politician INIT-X INIT-Y)
                                    (make-throbber INIT-X INIT-Y)
                                    (make-clock INIT-X INIT-Y))))
     (define m1 (make-metatoy empty))
     (define m2 (make-metatoy (list (make-throbber INIT-X INIT-Y))))
     (define m2-0 (make-metatoy (list (make-throbber INIT-X INIT-Y))))
     (define m2-1 (make-metatoy (list (make-throbber INIT-X INIT-Y))))
     (define m2-2 (make-metatoy (list (make-throbber INIT-X INIT-Y))))
     (define m2-3 (make-metatoy (list (make-throbber INIT-X INIT-Y))))
     (define m2-4 (make-metatoy (list (make-throbber INIT-X INIT-Y))))
     (define m2-5 (make-metatoy (list (make-throbber INIT-X INIT-Y))))
    
     (define m4 (make-metatoy (list (make-throbber INIT-X INIT-Y)
                                    (make-throbber INIT-X INIT-Y))))
    
     )
    (check-equal?
     (length (send m0 get-toys)) 3
     "there should be 3 toys in m0")
    (send m2-0 after-tick)
    (check-equal?
     (send (first (send m2-0 get-toys)) toy-data) 6
     "after one tick, the throbber radius should be 5+1-6")
    (send m2-1 after-mouse-event 0 0 "button-up")
    (check-equal?
     (send m2 add-to-scene EMPTY-CANVAS) (send m2-1 add-to-scene EMPTY-CANVAS)
     "after button-up event, m2 should be same in appearance")
    (send m2-2 after-mouse-event 0 0 "button-down")
    (check-equal?
     (send m2  add-to-scene EMPTY-CANVAS) (send m2-2  add-to-scene EMPTY-CANVAS)
     "after button-down outside throbber, m2 should be same in appearance")
    (send m2-3 after-mouse-event 0 0 "drag")
    (check-equal?
     (send m2 add-to-scene EMPTY-CANVAS) (send m2-3 add-to-scene EMPTY-CANVAS)
     "after drag outside throbber, m2 should be same in appearance")
    (send m2-4 after-mouse-event 0 0 "move")
    (check-equal?
     (send m2 add-to-scene EMPTY-CANVAS) (send m2-4 add-to-scene EMPTY-CANVAS)
     "after move event outside throbber, m2 should be same in appearance")
    (send m2-4 after-mouse-event 0 0 "leave")
    (check-equal?
     (send m2 add-to-scene EMPTY-CANVAS) (send m2-4 add-to-scene EMPTY-CANVAS)
     "after move event outside throbber, m2 should be same in appearance")
    (send m2-5 after-mouse-event 0 0 "enter")
    (check-equal?
     (send m2  add-to-scene EMPTY-CANVAS) (send m2-5  add-to-scene EMPTY-CANVAS)
     "after irelavent mouse event, m2 should be same in appearance")
    (send m1 after-key-event "t")
    (send m1 after-key-event "c")
    (send m1 after-key-event "p")
    (send m1 after-key-event "k")
    (check-equal?
     (length (send m1 get-toys)) 3
     "after key events the number of toys is 3")
 
  ))
