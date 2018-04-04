#lang racket
(require rackunit)
(require "extras.rkt")
(require "interface.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(provide Clock%)

;===============================================================================
;; CONSTANTS

;;Represents the width of the rectangle
(define RECTANGLE-WIDTH 40)

;;Represents the height of the rectangle
(define RECTANGLE-HEIGHT 20)

;;Image
;;Creates a rectangle with the mentioned width and height.
;; Having outline mode and blue color
(define RECTANGALE-IMAGE
  (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT "outline" "blue"))

;===============================================================================
;; THE CLOCK CLASS

;; Clocks start at the center of the screen which the tick component which
;; increases on each tick.
;; Clocks are selectable and draggable.

;; A Clock is a (new Clock% [x Integer][y Integer]
;;                        [selected? Boolean][prev-mx Integer][prev-my Integer]
;;                         [tick] )  ;; all parameters are optional.
;; A Clock represents a clock.
;;==============================================================================
(define Clock%
  (class* object% (Toy<%>)

    ;;==========================================================================
   ;Boolean describing if the clock is selected
   ;if the clock is selected i.e. mouse click is inside the clock then the value
   ; will be true else false. when the Clock is placed this value is false.
   (init-field [selected? false])
   ;;===========================================================================
   ;Int Int center of the Clock.
   ;where x is the x co-ordinate representing the center of the clock.
   ;where y is the y co-ordinate representing the center of the clock.
   ;the value of these fields are based on the length and width of the canavas.
   (init-field [x INIT-X] [y INIT-Y] )
   ;;=========================================================================== 
   ;;Int Int
   ;;prev-mx is the x component of the mouse pointer.
   ;;prev-my is the y component of the mouse pointer.
   ;;previous value of the mouse click when the pointer is placed inside the
    ;;Clock initially these values are set to 0 as the object is not selected.
   (init-field [prev-mx 0] [prev-my 0])
   ;;=========================================================================== 
   ;;PosInt
   ;;tick represents the time after which the Clock was created.
   (init-field [tick 0])
   ;;===========================================================================
   (super-new)
    
  ;;============================================================================
  ;;add-to-scene: Scene -> Scene
  ;;Given: a scene which is used for drawing or placing other objects
  ;;Returns: A rectangle having tick in text format at the center
  ;;         of the created rectangle which placed on the scene. So a new scene
  ;;         with the added componenets is shown.
  ;;EXAMPLE: Check the test cases.  
  ;; Strategy : Combine Simpler functions.
  (define/public (add-to-scene scene)
    (place-image (text (number->string tick) 10 "red") x y
                 (place-image RECTANGALE-IMAGE x y scene)))
  ;;============================================================================
  ;;after-tick: -> PosInt
  ;;GIVEN : No inputs are required. but the event after-tick triggers the function
  ;;RETURNS : New value of tick which is the increment of the previous tick value
  ;;EXAMPLE: (send (send (new Clock%) after-tick) toy-data)->1   
  ;;STRATEGY: use constructor template for Clock
    (define/public (after-tick)
      (new Clock%
           [x x]
           [y y]
           [selected? selected?]
           [prev-mx prev-mx]
           [prev-my prev-my]
           [tick (+ 1 tick)]
           ))
    ;;==========================================================================
    ;;after-button-down:  PosInt PosInt-> Clock
    ;;GIVEN : Two values represeting the mouse location in co-ordinate system
    ;;        where mx is the x-component and my is the y-component
    ;;RETURNS : this clock with its selected value turned to true if the click
    ;;          was inside the area of the Clock.
    ;;EXAMPLE: (send (send (new Clock%) after-button-down INIT-X INIT-Y)
    ;;          for-testing:is-selected?) -> true
    ;;STRATEGY: Cases on whether the event is in the Clock
    (define/public (after-button-down mx my)
      (if (in-mini-toy? mx my)
          (new Clock%
           [x x]
           [y y]
           [selected? true]
           [prev-mx mx]
           [prev-my my]
           [tick tick]
           )
          this))
    ;;==========================================================================
    ;;in-mini-toy?:PosInt PosInt -> Boolean
    ;;GIVEN : Two values represeting the mouse location in co-ordinate system
    ;;        where mx is the x-component and my is the y-component
    ;;RETURNS : Boolean true if the location of the mouse pointer falls within 
    ;;          the area of the Clock.
    ;;EXAMPLE: (send (new Clock%) in-mini-toy? INIT-X INIT-Y) -> true
    ;;STRATEGY: Combine Simpler Functions.
    (define (in-mini-toy? mx my)
      (and (within-x mx) (within-y my)))
    ;;==========================================================================
    ;;within-x: PosInt->Boolean
    ;;GIVEN : x component of the mouse location.
    ;;RETURNS : Boolean if the x component is within the boundary of the rectangle
    ;;EXAMPLE: (send (new Clock%) within-x INIT-X )-> true.
    ;;STRATEGY: Combine Simpler Functions.
    (define (within-x mx)
      (and (<= mx (+ x RECTANGLE-WIDTH)) (>= mx (- x RECTANGLE-WIDTH))))
    ;;==========================================================================
    ;;within-y : PosInt->Boolean
    ;;GIVEN : y component of the mouse location.
    ;;RETURNS : Boolean if the y component is within the boundary of the rectangle
    ;;EXAMPLE: (send (new Clock%) within-y (+ INIT-y 5))-> true.
    ;;STRATEGY:Combine Simpler Functions.
    (define (within-y my)
      (and (<= my (+ y RECTANGLE-HEIGHT)) (>= my (- y RECTANGLE-HEIGHT))))
    ;;==========================================================================
    ;;after-button-up:PosInt PosInt-> Clock
    ;;GIVEN : Two PosInt values represeting the mouse location in co-ordinate 
    ;;        system where mx is the x-component and my is the y-component
    ;;RETURNS : this Clock if the clock was seleted previously with seleted?
    ;;          component turned to false.
    ;;EXAMPLE : (send (send (new Clock% [selected? true])
    ;;           after-button-up INIT-X INIT-Y)
    ;;           for-testing:is-selected?) -> false
    ;;STRATEGY: Use constructor template for Clock
    (define/public (after-button-up  mx my)
      (new Clock%
           [x x]
           [y y]
           [selected? false]
           [prev-mx mx]
           [prev-my my]
           [tick tick]
           ))
    ;;==========================================================================
    ;;after-key-event:KeyInput-> Clock
    ;;GIVEN : KeyInput
    ;;RETURNS : this Clock
    ;;EXAMPLE: (send (send (new Clock%) after-key-event "n") toy-x) ->INIT-X
     (define/public (after-key-event kev) this)
    ;;=========================================================================
    ;; after-drag:PosInt PosInt->Clock.
    ;;GIVEN : Two values represeting the mouse location in co-ordinate system
    ;;        where mx is the x-component and my is the y-component
    ;;RETURNS : this Clock with selected turned to true if the clock was seleted?
    ;;EXAMPLE :(send (send (new Clock% [selected? true][prev-mx INIT-X]
    ;;          [prev-my INIT-Y] ) after-drag (+ INIT-X 5)
    ;;           INIT-Y) toy-x) ->(+ INIT-X 5)
    ;;STRATEGY:Cases on whether the the Clock is selected
    (define/public (after-drag mx my)
      (if selected?
          (new Clock%
           [x (check-x-drag mx)]
           [y (check-y-drag my)]
           [selected? selected?]
           [prev-mx mx]
           [prev-my my]
           [tick tick]
           )
          this))
    ;;==========================================================================
    ;;after-move: PosInt PosInt-> Clock
    ;;GIVEN : Two values represeting the mouse location in co-ordinate system
    ;;        where mx is the x-component and my is the y-component
    ;;RETURNS : this Clock as no action is required on move
    ;;EXAMPLE: (send (send (new Clock%) after-move ) toy-x) ->INIT-X
     (define/public (after-move mx my)
      this)
    ;;==========================================================================
    ;;toy-x: -> Int
    ;;GIVEN : No inputs required.
    ;;RETURNS : the x-component of the center of the circle.
    ;;EXAMPLE : (send (new Clock%) toy-x) -> INIT-X
    (define/public (toy-x )
      x)
    ;;==========================================================================
    ;;toy-y: -> Int
    ;;GIVEN : No Inputs required
    ;;RETURNS : the y-component of the center of the circle.
    ;;EXAMPLE : (send (new Clock%) toy-y) -> INIT-Y
    (define/public (toy-y )
       y)
    ;;=========================================================================
    ;;toy-data: -> PosInt
    ;;GIVEN : No inputs are required
    ;;RETURNS : the tick value of the clock.
    ;;EXAMPLE : (send (new Clock%) toy-data) -> 0
    (define/public (toy-data )
          tick)
    ;;=========================================================================
    ;; check-x-drag : Int -> Int
    ;; GIVEN :x component of mouse coordinates
    ;; RETURN : Int value indicating the new center after the mouse drag
    ;; EXAMPLE : (send (send (new Clock%[selected? true][prev-mx INIT-X]
    ;;            [prev-my INIT-Y]) check-x-drag (+ INIT-X 5)) toy-x)
    ;;            ->(+ INIT-X 5) 
    ;;STRATEGY : Conditions on Clocks prev-mx attribute
    (define (check-x-drag mx)
      (cond
        [(> mx prev-mx) (+ x (- mx prev-mx))]
        [(< mx prev-mx) (- x (- prev-mx mx))]
        [else x]))
    ;;==========================================================================
    ;;check-y-drag : Int => Int
    ;; GIVEN :  y component of mouse coordinates.
    ;; RETURN : Int representing the new center of Clock after the mouse drag.
    ;;;; EXAMPLE : (send (send (new Clock%[selected? true][prev-mx INIT-X]
    ;;            [prev-my INIT-Y]) check-y-drag (- INIT-Y 5)) toy-y)
    ;;            ->(- INIT-Y 5) 
    ;;STRATEGY : Conditions on Circles's prevmy attribute
    (define (check-y-drag my)
      (cond
        [(> my prev-my) (+ y (- my prev-my))]
        [(< my prev-my) (- y (- prev-my my))]
        [else y] ))

   ;;==========================================================================
    ;;methods for testing
    ;; Strategy: return values stored in the field of this object
    ;;EXAMPLE: (send (new Clock%[selected? true][prev-mx INIT-X]
    ;;            [prev-my INIT-Y]) for-testing:is-selected?)
    ;;           -> true
    ;;(send (new Clock%[selected? true][prev-mx INIT-X]
    ;;            [prev-my INIT-Y]) for-testing:prevmx)
    ;;           -> INIT-X
    ;;(send (new Clock%[selected? true][prev-mx INIT-X]
    ;;            [prev-my INIT-Y]) for-testing:prevmy )
    ;;            ->INIT-Y
    (define/public (for-testing:is-selected? ) selected?)
    (define/public (for-testing:prevmx ) prev-mx)
    (define/public (for-testing:prevmy ) prev-my)
    
    ))

;;=============================================================================
;;TEST CASES
(begin-for-test
  (local
    ((define clock0 (new Clock%))
     (define clock1 (send clock0 after-button-down INIT-X INIT-Y))
     (define clock2 (send clock1 after-button-up (+ INIT-X 10) (+ INIT-Y 5)))
     (define clock3 (send clock1 after-drag (+ INIT-X 10) (+ INIT-Y 5)))
     (define clock4 (send clock0 after-tick))
     (define clock5 (new Clock% [selected? true]
                         [prev-mx INIT-X] [prev-my INIT-Y]))
     )

    (check-equal? (send clock0 add-to-scene EMPTY-CANVAS)
                 (send clock1 add-to-scene EMPTY-CANVAS)
                 "Both scenes are same")
    (check-equal? (send (send clock0 after-drag 100 200) toy-x) INIT-X
                  "after draging when unselected, clock should not move")
    (check-equal? (send clock1 for-testing:is-selected?) true
                  "the Clock is selected")
    (check-equal? (send (send clock0 after-button-down 300 300)
                        for-testing:is-selected?) false
                     "Clock is not selected")
    (check-equal? (send clock2 for-testing:is-selected?) false
                  "the Clock is deselected")
    (check-equal? (send clock3 toy-x) (+ INIT-X 10)
                  "the Clock is dragged")
    (check-equal? (send clock3 toy-y) (+ INIT-Y 5)
                  "the Clock is dragged")
    (check-equal? (send clock3 after-move 10 10) clock3
                  "the Clock is the same")
    (check-equal? (send clock3 after-key-event "c") clock3
                  "the Clock is the same")
    (check-equal? (send (send (send clock0 after-tick)after-tick) toy-data)
                  (send (send clock4 after-tick) toy-data)
                  "the clock after tick")
    (check-equal? (send clock1 for-testing:prevmx) INIT-X
                  "The previous value of the mouse click")
    (check-equal? (send clock1 for-testing:prevmy) INIT-Y
                  "The previous value of the mouse click")
    (check-equal? (send (send clock5 after-drag 100 200) toy-x) 100
                  "after draging, the clock5 should end up x=100")
    (check-equal? (send (send clock5 after-drag INIT-X INIT-Y) toy-x) INIT-X
                  "if mx,my no change, x,y should not change")))
  