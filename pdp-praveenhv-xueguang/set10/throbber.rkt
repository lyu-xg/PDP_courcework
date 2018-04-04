#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "interfaces.rkt")
(require "constants.rkt")

(provide Throbber%)

(define MIN-RADIUS 5)
(define MAX-RADIUS 20)
(define INIT-RADIUS MIN-RADIUS)
(define GROW-SPEED 1)


;;==============================================================================
;; THROBBER CLASS
;;Constructor Template:
;; (new Throbber% [x Integer][y Integer][selected? Boolean][growing? Boolean]
;;                [prev-mx Integer] [prev-my Integer] [tick Integer]
;;                [radius Integer]) the values to these parameters are optional
;;A Throbber represents a throbber on the canavas.
(define Throbber%
  (class* object% (Toy<%>)
    
    ;;x denotes x-component of the center of the throbber similarly y denotes
    ;;the y-component of the center of the throbber taken in graphical
    ;;co-ordinate system.
    (init-field x y)
    
    ;;selected? denotes if the throbber is selected default value is false.
    ;;the value of selected changes based on the mouse events.
    ;;growing? denotes if the throbber is expanding default value is false.
    (init-field [selected? false]
                [growing? false])
    ;;prev-mx denotes the x-component of previous mouse event which resulted to
    ;;the throbber getting selected similarly prev-my denotes the y-component
    ;;of previous mouse event which resulted to the throbber getting selected.
    ;;both are set to zero as the throbber is not selected by the mouse.
    (init-field [prev-mx 0] [prev-my 0])
    
    ;;==========================================================================
    ;; the intial radius of the throbber.
    (init-field [radius INIT-RADIUS])
    
    (field [THROBBER-IMG-NOT-SELECTED (circle radius "solid" "green")])
    
    ;;==========================================================================
    ;;The representation of the throbber as a image on the scene when it is
    ;;selected. The resultant image is a circle with green outline.
    ;;having the mentioned radius
    (field [THROBBER-IMG-SELECTED (circle radius "outline" "green")])
    
    ;;==========================================================================
    (super-new)
    ;;==========================================================================
    ;;toy-x  -> Int
    ;;GIVEN : No inputs are required
    ;;EXAMPLE: (send (new Throbber%) toy-x)-> INIT-X
    ;;RETURNS : the x-component of the center of the Throbber
    ;;Strategy: return a field
    (define/public (toy-x) x)
    ;;==========================================================================
    ;;toy-y: -> Int
    ;;GIVEN : No inputs are required
    ;;EXAMPLE: (send (new Throbber%) toy-y)-> INIT-Y
    ;;RETURNS : the y-component of the center of the Throbber
    ;;Strategy: return a field
    (define/public (toy-y) y)
    ;;==========================================================================
    ;;toy-data: -> Int
    ;;GIVEN : No inputs are required
    ;;EXAMPLE: (send (new Throbber%) toy-data) -> MIN-RADIUS
    ;;RETURNS : The radius of the Throbber.
    ;;Strategy: return a field
    (define/public (toy-data) radius)
    ;;==========================================================================
    ;; after-drag: Int Int  -> Void
    ;;GIVEN : mx and my represents the x and y components of the mouse
    ;;        co-ordinates respectively.
    ;;EFFECT: Updates the throbber to the state it should be after a mouse event
    ;;        drag.
    ;;        The position of the throbber is updated if the throbber was selected.
    ;;        certain other fields like selected? prev-mx and prev-my are also
    ;;        updated to allow smooth drag. the throbber is returned as it is
    ;;        if it is not selected.
    ;;Strategy:  Use cases based on whether the Throbber is selected.
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (+ x (- mx prev-mx)))
            (set! y (+ y (- my prev-my)))
            (set! selected? true)
            (set! prev-mx mx)
            (set! prev-my my))
          9))
    ;;==========================================================================
    ;;add-to-scene: scene -> scene.
    ;;GIVEN : a scene which allows for other images to be placed.
    ;;RETURNS : a scene like the given one,
    ;;          but contains the throbber.
    ;;EXAMPLE : see the test cases for example.
    ;;STRATEGY:Use cases based on whether the Throbber is selected.
    (define/public (add-to-scene scene)
      (place-image
       (circle radius (if selected? "outline" "solid") "green")
       x y scene))
    ;;==========================================================================
    ;;after-button-down: Int Int -> Void
    ;;GIVEN : mx and my represents the x and y components of the mouse
    ;;        co-ordinates respectively.
    ;;EFFECT:  Updates the throbber to the state it should follow after a button
    ;;         down.
    ;;          the throbbers selected component is modified based on event.
    ;;STRATEGY: Use cases based on whether the mouse pointer is
    ;;          inside the throbber
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
          (begin
            (set! selected? true)
            (set! prev-mx mx)
            (set! prev-my my))               
          10))
    ;;==========================================================================
    ;;after-tick:  -> Void
    ;;GIVEN : No inputs
    ;;EFFECT : updates the throbber to the state it should be following the tick.
    ;;         The radius of the throbber is updated based on limits.
    ;;STRATEGY: updating the radius after the tick.
    (define/public (after-tick)
      (set! growing? (growing-after-tick))
      (set! radius (radius-after-tick)))
    ;;==========================================================================
    ;; after-key-event : KeyEvent -> Void
    ;; EFFECT: updates the throbber to the state it should have
    ;; following the given key event
    ;; DETAILS: a throbber ignores key events
    ;;Strategy: return Void
    (define/public (after-key-event kev) 80)
    ;;==========================================================================
    ;; after-button-up : NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a button-up event
    ;; EFEECT: Updates the throbber to the state it should have
    ;; to the state it should have following the button-up event.
    ;; If the throbber is selected, then unselect it.
    ;; STRATEGY: Cases on whether the event is in the throbber.
    (define/public (after-button-up mx my) (set! selected? false))
    ;;==========================================================================
    ;;after-move:int int-> Throbber
    ;;GIVEN : mx and my represents the x and y components of the mouse
    ;;        co-ordinates respectively.
    ;;RETURNS : The same Throbber is returned.
    ;;EXAMPLE : (send (send (new Throbber%) after-move 120 120)toy-x)-> INIT-X
    ;;Strategy: return Void
    (define/public (after-move mx my) 567)
    ;;==========================================================================
    ;;in-throbber?:Int Int-> Boolean
    ;;GIVEN : mx and my represents the x and y components of the mouse
    ;;        co-ordinates respectively.
    ;;RETURNS : Boolean . True if the value of mx and my falls within the
    ;;          boundary of the Throbber.
    ;;EXAMPLE : (send (new Throbber%) in-throbber? INIT-X INIT-Y) -> true.
    ;;STRATEGY: Combine Simpler functions.
    (define (in-throbber? mx my)
      (<= (euclidean-distance mx my x y) radius))
    ;;==========================================================================
    ;;radius-after-tick:-> PosInt
    ;;GIVEN : No inputs required
    ;;RETURNS : New Radius of the throbber
    ;;EXAMPLE : (send (new Throbber%) radius-after-tick) -> 6
    ;;STRATEGY: Conditions based on current radius and MAX/MIN-RADIUS
    (define (radius-after-tick)
      (cond
        [(<= radius MIN-RADIUS) (+ radius GROW-SPEED)]
        [(>= radius MAX-RADIUS) (- radius GROW-SPEED)]
        [else ((if growing? + -) radius GROW-SPEED)]))
    ;;==========================================================================
    ;;growing-after-tick:-> Boolean
    ;;GIVEN : No Inputs required
    ;;RETURNS : Boolean . True if current radius is within min max limits.
    ;;EXAMPLE : (send (new Throbber%[growing? true][radius 22])
    ;;           growing-after-tick ) -> false.
    ;;STRATEGY: Conditions based on current radius and MAX/MIN-RADIUS
    (define (growing-after-tick)
      (if (or (<= radius MIN-RADIUS)
              (>= radius MAX-RADIUS))
          (not growing?)
          growing?))
    ;;==========================================================================
    ;;for tests
    ;; Strategy: return values stored in the field of this object
    ;;EXAMPLE: (send (new Throbber%) for-test:is-selected? )->false
    ;;EXAMPLE: (send (new Throbber%) for-test:is-growing? )->false
    ;;EXAMPLE: (send (new Throbber%) for-test:prev-my ) -> 0
    ;;EXAMPLE: (send (new Throbber%) for-test:prev-mx ) -> 0
    
    (define/public (for-test:is-selected?) selected?)
    
    (define/public (for-test:is-growing?) growing?)
    
    (define/public (for-test:prev-my) prev-my)
    
    (define/public (for-test:prev-mx) prev-mx)
    
    ))

;;==============================================================================
;;TEST CASES.


(begin-for-test
  (local
    ((define th (new Throbber% [x INIT-X] [y INIT-Y])))
    (send th after-button-down INIT-X INIT-Y)
    (check-equal?
     (send th for-test:prev-my) 300
     "on button-down event, my co-ordinate shouldn change")
    (check-equal?
     (send th for-test:prev-mx) 250
     "on button-down event, mx co-ordinate shouldn change")
    (check-equal?
     (send th for-test:is-selected?) true "the throbber is selected")
    (send th after-tick)
    (check-equal?
     (send th toy-data) 6 "the radius increases")
    (check-equal?
     (send th add-to-scene EMPTY-CANVAS)
     (place-image (circle 6 "outline" "green") INIT-X INIT-Y EMPTY-CANVAS)
     "same scene")
    (send th after-move 150 50)
    (check-equal?
     (send th toy-x) INIT-X "the x remains same")
    (check-equal?
     (send th toy-y) INIT-Y "the y remains same")
    (send th after-key-event "t")
    (check-equal?
     (send th toy-x) INIT-X "the x remains same")
    (check-equal?
     (send th toy-y) INIT-Y "the y remains same")
    (send th after-button-up 120 120)
    (send th after-button-down (+ INIT-X 80) (+ INIT-Y 80))
    (check-equal?
     (send th for-test:is-selected?) false "throbber is not selected")
    (send th after-button-up 120 120)
    (check-equal?
     (send th add-to-scene EMPTY-CANVAS)
     (place-image (circle 6 "solid" "green") INIT-X INIT-Y EMPTY-CANVAS)
     "same scene")
    (send th after-drag 10 20)
    (check-equal?
     (send th toy-x) INIT-X "Not Dragged")
    
    )
  (local
    ((define th (new Throbber% [x INIT-X] [y INIT-Y] [radius 19] )))
    (send th after-tick)
    (check-equal?
     (send th for-test:is-growing?) false "growing is false as it reached max")
    (check-equal?
     (send th toy-data) 18 "the radius decreases")
    (send th after-tick)
    (check-equal?
     (send th for-test:is-growing?) false "growing is false")
    
    
    )
  
  (local
    ((define th (new Throbber% [x INIT-X] [y INIT-Y] [radius 5] )))
    (send th after-tick)
    (check-equal?
     (send th for-test:is-growing?) true "growing is true as it reached min")
    (check-equal?
     (send th toy-data) 6 "the radius increases")
    (send th after-tick)
    (check-equal?
     (send th for-test:is-growing?) true "growing is true")
    )
  
  (local
    ((define th (new Throbber% [x INIT-X] [y INIT-Y] [radius 20][growing? true])))
    (send th after-tick)
    (check-equal?
     (send th for-test:is-growing?) false "growing is true as it reached max")
    (check-equal?
     (send th toy-data) 19 "the radius decreases")
    (send th after-tick)
    (check-equal?
     (send th for-test:is-growing?) false "growing is false")
    )
  
  (local
    ((define th (new Throbber% [x INIT-X] [y INIT-Y] [selected? true]
                     [prev-mx INIT-X] [prev-my INIT-Y])))
    
    (send th after-button-up INIT-X INIT-Y)
    (check-equal?
     (send th for-test:is-selected?) false "throbber is un selected"
     ))
  
  (local
    ((define th (new Throbber% [x INIT-X] [y INIT-Y] [selected? true]
                     [prev-mx INIT-X] [prev-my INIT-Y])))
    (send th after-drag (+ INIT-X 5) INIT-Y)
    (check-equal?
     (send th toy-x) (+ INIT-X 5) "throbber is moved right")
    (send th after-drag (- INIT-X 5) INIT-Y)
    (check-equal?
     (send th toy-x) (- INIT-X 5)  "throbber is moved left")
    (send th after-drag INIT-X  (+ INIT-Y 5))
    (check-equal?
     (send th toy-y) (+ INIT-Y 5) "throbber is moved up")
    (send th after-drag INIT-X  (- INIT-Y 5))
    (check-equal?
     (send th toy-y) (- INIT-Y 5) "throbber is moved down"))
  
  
  
  )



