#lang racket
(require rackunit)
(require "extras.rkt")
(require "interface.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(provide Throbber%)

;;==============================================================================
;; CONSTANTS

;;Min radius is the minimum value of radius which a throbber can take
;;Max radius is the maximum value of radius which a throbber can take
;; Actual radius is the initial radius of the throbber.
(define MIN-RADIUS 5)
(define MAX-RADIUS 20)
(define INIT-RADIUS MIN-RADIUS)

;; the value with which the throbber must expand or contract.
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
    
    ;;==========================================================================
    ;;selected? denotes if the throbber is selected default value is false.
    ;;the value of selected changes based on the mouse events.
    ;;growing? denotes if the throbber is expanding default value is false.
    (init-field [selected? false]
                [growing? false])
    ;;==========================================================================
    ;;x denotes x-component of the center of the throbber similarly y denotes
    ;;the y-component of the center of the throbber taken in graphical
    ;;co-ordinate system.
    (init-field [x INIT-X] [y INIT-Y])
    
    ;;==========================================================================
    ;;prev-mx denotes the x-component of previous mouse event which resulted to
    ;;the throbber getting selected similarly prev-my denotes the y-component
    ;;of previous mouse event which resulted to the throbber getting selected.
    ;;both are set to zero as the throbber is not selected by the mouse.
    (init-field [prev-mx 0] [prev-my 0])
    
    ;;==========================================================================
    ;; the intial radius of the throbber.
    (init-field [radius INIT-RADIUS])
    
    ;;==========================================================================
    ;;The representation of the throbber as a image on the scene when it is
    ;;not selected . The resultant image is a circle with solid green color
    ;;having the mentioned radius
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
    ;EXAMPLE: (send (new Throbber%) toy-x)-> INIT-X
    ;;RETURNS : the x-component of the center of the Throbber
    (define/public (toy-x) x)
    ;;==========================================================================
    ;;toy-y: -> Int
    ;;GIVEN : No inputs are required
    ;;EXAMPLE: (send (new Throbber%) toy-y)-> INIT-Y
    ;;RETURNS : the y-component of the center of the Throbber
    (define/public (toy-y) y)
    ;;==========================================================================
    ;;toy-data: -> Int
    ;;GIVEN : No inputs are required
    ;;EXAMPLE: (send (new Throbber%) toy-data) -> MIN-RADIUS
    ;;RETURNS : The radius of the Throbber.
    (define/public (toy-data) radius)
    ;;==========================================================================
    ;; after-drag: Int Int  -> Throbber
    ;;GIVEN : mx and my represents the x and y components of the mouse
    ;;        co-ordinates respectively.
    ;;RETURNS : New Throbber whose centers are displaced if the
    ;;         Throbber is selected. Movement depends on the mouse co-cordinates
    ;;          else  same THrobber is returned.
    ;;EXAMPLE: (send (send (new Throbber%[selected? true]
    ;;          [prev-mx INIT-X][prev-my INIT-Y])
    ;;          after-drag (+ INIT-X 1) INIT-Y)) toy-x)-> (+ INIT-X 1)
    ;;STRATEGY: Use cases based on whether the Throbber is selected.
    (define/public (after-drag mx my)
      (if selected?
          (new Throbber%
               [x (x-after-drag mx)]
               [y (y-after-drag my)]
               [selected? true]
               [growing? growing?]
               [radius radius]
               [prev-mx mx]
               [prev-my my])
          this))
    ;;==========================================================================
    ;;add-to-scene:scene-> scene
    ;;GIVEN : a scene which allows for other images to be placed.
    ;;RETURNS : a scene like the given one,
    ;;          but contains the throbber.
    ;;EXAMPLE : see the test cases for example.
    ;;STRATEGY:Use cases based on whether the Throbber is selected.
    (define/public (add-to-scene scene)
      (if selected?
          (place-image THROBBER-IMG-SELECTED x y scene)
          (place-image THROBBER-IMG-NOT-SELECTED x y scene)))
    ;;==========================================================================
    ;;after-button-down: Int Int -> Throbber
    ;;GIVEN : mx and my represents the x and y components of the mouse
    ;;        co-ordinates respectively.
    ;;RETURNS : New Throbber if the mouse co-ordinates were inside the throbber.
    ;;          the throbbers selected component is modified based on event.
    ;;EXAMPLE : (send (send (new Throbber%[selected? false])
    ;;          after-button-down (+ INIT-X 1) INIT-Y)) for-test:is-selected?)
    ;;           -> true
    ;;STRATEGY: Use cases based on whether the mouse pointer is
    ;;          inside the throbber
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
          (new Throbber%
               [x x]
               [y y]
               [selected? true]
               [growing? growing?]
               [radius radius]
               [prev-mx mx]
               [prev-my my]
               )
          this))
    
    ;;==========================================================================
    ;;after-button-up:Int Int -> Throbber
    ;;GIVEN : mx and my represents the x and y components of the mouse
    ;;        co-ordinates respectively.
    ;;RETURNS : New Throbber if the mouse co-ordinates were inside the throbber.
    ;;          the throbbers selected component is modified based on event.
    ;;EXAMPLE: (send (send (new Throbber%[selected? false])
    ;;          after-button-up (+ INIT-X 1) INIT-Y)) for-test:is-selected?)
    ;;           -> false
    ;;STRATEGY: Use comstructor template for Throbber
    (define/public (after-button-up mx my)
      (new Throbber%
           [x x]
           [y y]
           [selected? false]
           [growing? growing?]
           [radius radius]
           [prev-mx mx]
           [prev-my my]))
    ;;==========================================================================
    ;;after-tick:  -> Throbber
    ;;GIVEN : No inputs
    ;;RETURNS : New Throbber with new radius
    ;;EXAMPLE : (send (send (new Throbber%) after-tick)toy-data)-> 6
    ;;STRATEGY: use constructor template for Throbber
    (define/public (after-tick)
      (new Throbber%
           [x x] [y y]
           [selected? selected?]
           [growing? (growing-after-tick)]
           [radius (radius-after-tick)]
           [prev-mx prev-mx]
           [prev-my prev-my]))
    ;;==========================================================================
    ;;after-key-event:KeyInput-> Throbber
    ;;GIVEN : KeyInput
    ;;RETURNS : The same Throbber is returned.
    ;;EXAMPLE: (send (send (new Throbber%) after-key-event "p") toy-x)-> INIT-X
    (define/public (after-key-event kev) this)
    
    ;;==========================================================================
    ;;after-move:int int-> Throbber
    ;;GIVEN : mx and my represents the x and y components of the mouse
    ;;        co-ordinates respectively.
    ;;RETURNS : The same Throbber is returned.
    ;;EXAMPLE : (send (send (new Throbber%) after-move 120 120)toy-x)-> INIT-X
    (define/public (after-move mx my) this)
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
    ;;growing-after-tick:-> Boolean.
    ;;GIVEN : No Inputs required.
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
    ;;x-after-drag:Int-> Int
    ;;GIVEN : mx representing the x component of the mouse pointer
    ;;RETURNS : new x-component for the center of the throbber
    ;;EXAMPLE : (send (new Throbber%[selected? true]
    ;;           [prev-mx INIT-X][prev-my INIT-Y]) x-after-drag (+ INIT-X 5))
    ;;           -> (+ INIT-X 5)
    ;;STRATEGY: Conditions based on new and previous mouse x co-ordinate
    (define (x-after-drag mx)
      (cond
        [(> mx prev-mx) (+ x (- mx prev-mx))]
        [(< mx prev-mx) (- x (- prev-mx mx))]
        [else x]))
    ;;==========================================================================
    ;;y-after-drag:Int-> Int
    ;;GIVEN : my representing the y component of the mouse pointer
    ;;RETURNS : new y-component for the center of the throbber
    ;;EXAMPLE : (send (new Throbber%[selected? true]
    ;;           [prev-mx INIT-X][prev-my INIT-Y]) y-after-drag (+ INIT-Y 5))
    ;;           -> (+ INIT-Y 5)
    ;;STRATEGY: Conditions based on new and previous mouse y co-ordinate
    (define (y-after-drag my)
      (cond
        [(> my prev-my) (+ y (- my prev-my))]
        [(< my prev-my) (- y (- prev-my my))]
        [else y] ))
    
    ;;=========================================================================
    ;;for tests
    ;; Strategy: return values stored in the field of this object
    ;;EXAMPLE: (send (new Throbber%) for-test:is-selected? )->false.
    ;;EXAMPLE: (send (new Throbber%) for-test:is-growing? )->false.
    (define/public (for-test:is-selected?)
      selected?
      )
    (define/public (for-test:is-growing?)
      growing?
      )
    
    ))

;;==============================================================================
;;TESTS
(begin-for-test
  (local
    ((define throb0 (new Throbber%))
     (define throb1 (send throb0 after-button-down INIT-X INIT-Y))
     (define throb2 (send throb0 after-button-down  (+ INIT-X 5) (+ INIT-Y 5)))
     (define throb3 (send throb1 after-drag (+ INIT-X 1) (+ INIT-Y 1)))
     (define throb4 (send throb1 after-button-up (+ INIT-X 3) (+ INIT-Y 3)))
     (define throb5 (send throb0 after-button-down (+ INIT-X 2) (+ INIT-Y 2)))
     (define throb6 (new Throbber% [x 20][y 20][radius 6] ))
     (define throb7 (new Throbber% [x 20][y 20][radius 20] ))
     (define throb8 (new Throbber% [x 20][y 20][radius 19] ))
     (define throb9 (new Throbber% [x 20][y 20][radius 25][growing? true]))
     (define throb10 (send (send throb0 after-tick) after-tick))
     (define throb11 (new Throbber% [radius 10] [growing? false]))
     (define throb12 (send throb11 after-tick))
     (define throb13 (new Throbber% [selected? true]
                          [prev-mx INIT-X] [prev-my INIT-Y]))
    )
    (check-equal? (send throb1 for-test:is-selected?) true
                  "the Throbber is selected")
    (check-equal? (send (send throb0 after-drag 100 200)
                        add-to-scene EMPTY-CANVAS)
                  (send throb0 add-to-scene EMPTY-CANVAS)
                  "if not selected, dragging should not change the appearance")
    (check-equal? (send throb2 for-test:is-selected?) false
                  "the throbber is not selected")
    (check-equal? (send throb10 toy-data) 7
                  "after two ticks, throb10 should have a radius of 5+2=7")
    (check-equal? (send throb12 toy-data) 9
                  "after one tick, throb12 should have a radius of 10-1=9")
    (check-equal? (send throb3 toy-x) 251
                  "the throbber is dragged")
    (check-equal? (send (send throb13 after-drag 100 200) toy-x) 100
                  "after drag, throb13 should end up at x=100")
    (check-equal? (send (send throb13 after-drag INIT-X INIT-Y) toy-x) INIT-X
                  "if mx,my have no changes, throb13's x should not change")
    (check-equal? (send throb3 toy-y) 301
                  "the throbber is dragged")

    (check-equal? (send throb4 for-test:is-selected?) false
                  "the throbber deselected")

    (check-equal? (send throb4 add-to-scene EMPTY-CANVAS)
                  (send throb0 add-to-scene EMPTY-CANVAS)
                  "both  Throbbers are same in appearance"
                  )
    (check-equal? (send throb1 add-to-scene EMPTY-CANVAS)
                  (send throb5 add-to-scene EMPTY-CANVAS)
                  "both  Throbbers are same in appearance")
    (check-equal? (send (send throb0 after-tick) toy-data)
                  (send throb6 toy-data)
                  "the radius expands after tick")
    (check-equal? (send (send throb7 after-tick) toy-data)
                  (send throb8 toy-data)
                  "the radius contracts after tick")
    (check-equal? (send throb7 after-key-event "r")
                  throb7
                  "the throbber is the same even after the event")
    (check-equal? (send throb7 after-move 20 20)
                  throb7
                  "the throbber is the same even after the event")
    (check-equal? (send (send throb0 after-tick) for-test:is-growing?)
                  true
                  "the radius expands after tick")
    (check-equal? (send (send throb9 after-tick) for-test:is-growing?)
                  false
                  "the radius contracts after tick")))