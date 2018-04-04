#lang racket
(require rackunit)
(require "extras.rkt")
(require "interfaces.rkt")
(require "constants.rkt")
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
    (init-field [x INIT-X] [y INIT-Y])
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
    ;;after-tick: -> Void
    ;;GIVEN : No inputs are required. but the event after-tick triggers the function
    ;;EFFECT : Updates the clock to the state which follows the tick event.
    ;;        Clock value tick is incremented.
    ;;STRATEGY: Combine simple functions
    (define/public (after-tick)
      (set! tick (+ 1 tick)))
    ;;==========================================================================
    ;;after-button-down:  PosInt PosInt-> Void
    ;;GIVEN : Two values represeting the mouse location in co-ordinate system
    ;;        where mx is the x-component and my is the y-component
    ;;EFFECT : Updates the clock to the state which should follow the button
    ;;        down event. Clock with its selected value turned to true
    ;;        if the click was inside the area of the Clock and prev mx my
    ;;        values are updated
    ;;STRATEGY: Cases on whether the event is in the Clock
    (define/public (after-button-down mx my)
      (if (in-mini-toy? mx my)
          (begin (set! selected? true)
                 (set! prev-mx mx)
                 (set! prev-my my))
          24))
    ;;==========================================================================
    ;;in-mini-toy?:PosInt PosInt -> Boolean
    ;;GIVEN : Two values represeting the mouse location in co-ordinate system
    ;;        where mx is the x-component and my is the y-component
    ;;RETURNS : Boolean true if the location of the mouse pointer falls within 
    ;;          the area of the Clock.
    ;;EXAMPLE: (send (new Clock%) in-mini-toy? INIT-X INIT-Y) -> true
    ;;STRATEGY: Combine Simpler Functions.
    (define (in-mini-toy? mx my)
      (and (< (abs (- mx x)) (/ RECTANGLE-WIDTH 2))
           (< (abs (- my y)) (/ RECTANGLE-HEIGHT 2))))
    ;;==========================================================================
    ;;after-button-up:PosInt PosInt-> Void
    ;;GIVEN : Two PosInt values represeting the mouse location in co-ordinate 
    ;;        system where mx is the x-component and my is the y-component
    ;;EFFECT : Udates the clock to the state which should follow the mouse event
    ;;         Attributes prev-mx prev-my tick and selected is updated.
    ;;STRATEGY: Use constructor template for Clock
    (define/public (after-button-up  mx my)
      (set! selected? false)
      (set! prev-mx mx)
      (set! prev-my my))
    ;;==========================================================================
    ;;after-key-event : KeyEvent -> Void
    ;;GIVEN : KeyInput
    ;;EFFECT : None
    ;;EXAMPLE: (send (send (new Clock%) after-key-event "n") toy-x) ->INIT-X
    ;;Strategy: return Void
    (define/public (after-key-event kev) 9)
    ;;=========================================================================
    ;; after-drag:PosInt PosInt-> Void
    ;;GIVEN : Two values represeting the mouse location in co-ordinate system
    ;;        where mx is the x-component and my is the y-component
    ;;EFFECT : Updates the clock to the state which should follow the drag event
    ;;         attributes like x y and prev-mx and prev-my are set.
    ;;STRATEGY:Cases on whether the the Clock is selected
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (+ x (- mx prev-mx)))
            (set! y (+ y (- my prev-my)))
            (set! prev-mx mx)
            (set! prev-my my)
            )
          65))
    ;;==========================================================================
    ;;after-move: PosInt PosInt-> Void
    ;;GIVEN : Two values represeting the mouse location in co-ordinate system
    ;;        where mx is the x-component and my is the y-component
    ;;RETURNS : Void
    ;;EFFECT: None
    ;;EXAMPLE: (send (send (new Clock%) after-move ) toy-x) ->INIT-X
    ;;Strategy: return Void
    (define/public (after-move mx my) 75)
    ;;==========================================================================
    ;;toy-x: -> Int
    ;;GIVEN : No inputs required.
    ;;RETURNS : the x-component of the center of the circle.
    ;;EXAMPLE : (send (new Clock%) toy-x) -> INIT-X
    ;;Strategy: return a field
    (define/public (toy-x) x)
    ;;==========================================================================
    ;;toy-y: -> Int
    ;;GIVEN : No Inputs required
    ;;RETURNS : the y-component of the center of the circle.
    ;;EXAMPLE : (send (new Clock%) toy-y) -> INIT-Y
    ;;Strategy: return a field
    (define/public (toy-y) y)
    ;;=========================================================================
    ;;toy-data: -> PosInt
    ;;GIVEN : No inputs are required
    ;;RETURNS : the tick value of the clock.
    ;;EXAMPLE : (send (new Clock%) toy-data) -> 0
    ;;Strategy: return a field
    (define/public (toy-data) tick)
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
    (
     (define clk (new Clock% [x INIT-X] [y INIT-Y]))
     (define clk-sel (new Clock% [x INIT-X] [y INIT-Y] [selected? true]) )
     (define make-clk (new Clock%))
     )
    
    (check-equal? (send clk add-to-scene EMPTY-CANVAS)
                  (send clk-sel add-to-scene EMPTY-CANVAS)
                  "Both scenes are same")
    (send clk after-button-up INIT-X INIT-Y)
    (check-equal? (send clk for-testing:is-selected? ) false
                  "clock is not selected")
    (send clk after-button-down 10 20)
    (check-equal? (send clk for-testing:is-selected?) false "clock is not selected")
    (send clk after-drag 10 20)
    (check-equal? (send clk for-testing:is-selected?) false "clock is not selected")
    (send clk after-button-down INIT-X INIT-Y)
    (check-equal? (send clk for-testing:is-selected?) true "clock not selected")
    (send clk after-tick)
    (check-equal? (send clk toy-data) 1 "clock advances by one")
    (send clk after-drag (+ INIT-X 5) INIT-Y)
    (check-equal? (send clk toy-x) (+ INIT-X 5) "clock was dragged right")
    (send clk after-drag (- INIT-X 5) INIT-Y)
    (check-equal? (send clk toy-x) (- INIT-X 5) "clock was dragged right")
    (send clk after-drag INIT-X (+ INIT-Y 5))
    (check-equal? (send clk toy-y) (+ INIT-Y 5) "clock was dragged down")
    (send clk after-drag INIT-X (- INIT-Y 5))
    (check-equal? (send clk toy-y) (- INIT-Y 5) "clock was dragged up")
    (send clk after-move INIT-X (- INIT-Y 5))
    (check-equal? (send clk toy-y) (- INIT-Y 5) "clock doesnt respond")
    (send clk after-key-event "t")
    (check-equal? (send clk toy-data) 1 "clock stays same")
    (send clk-sel after-button-down INIT-X INIT-Y)
    (check-equal? (send clk-sel for-testing:prevmx) INIT-X "clock stays same")
    (check-equal? (send clk-sel for-testing:prevmy) INIT-Y "clock stays same")
    )
  )



