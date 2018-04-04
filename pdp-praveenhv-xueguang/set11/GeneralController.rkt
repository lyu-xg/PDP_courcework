#lang racket
(require "extras.rkt")
(require "interfaces.rkt")
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
;===============================================================================
(provide GeneralController%)
;===============================================================================
(define HANDLER-SIDE 10)
(define HALF-HANDLER-SIDE (/ HANDLER-SIDE 2))
;===============================================================================
(define GeneralController%
  ;; GeneralController class is the super class of all controllers
  (class* object% (Controller<%>)
    ;===========================================================================
    (init-field model) ; Model
    ;; the model to which this controller is registered
    ;===========================================================================
    (init-field width height) ;; Int Int
    ;; the width and height of this controller on the canvas
    ;===========================================================================
    (init-field [particle 0]) ;; Particle
    ;; the local cache of the particle
    ;; true value will be published from model once initiated
    ;===========================================================================
    (init-field [x INIT-X] [y INIT-Y]) ;; Int Int
    ;; the position of this controller on the canvas
    ;===========================================================================
    (init-field [saved-mx 0] [saved-my 0]) ;; Int Int
    ;; saved mouse positions 
    ;===========================================================================
    (init-field [controller-selected? false])
    (init-field [handler-selected? false])
    ;===========================================================================
    (super-new)
    ;===========================================================================
    ;; receive-signal: Particle -> Void
    ;; EFFECT: receive a particle from the model and adjust controller 
    ;; NOTE: expected to be called upon registration of this object to its model
    ;;       and when there is an update on the particle
    ;; Strategy: update a field
    (define/public (receive-signal p)
      (set! particle p))
    ;===========================================================================
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted on it
    ;; STRATEGY: place the controller image centered at x, y
    (define/public (add-to-scene scene)
      (let ([controller-image
             (overlay/align "left" "top"
                            (handler-image)
                            (viewer-image))])
        (place-image controller-image x y scene)))
    ;===========================================================================
    ;; update-model : -> Void
    ;; EFFECT: update the model using current cache of the particle
    ;; Startegy: send model an particle to update
    (define/public (update-model)
      (send model update particle))
    ;===========================================================================
    ;; viewer-image : -> Image
    ;; RETURNS: the image of the controller (without outline and handler)
    ;; Strategy: combine simpler functions
    (define (viewer-image)
      (let ((the-data-image (data-image)))
        (overlay 
         the-data-image
         (rectangle width height "outline" "black"))))
    ;===========================================================================
    ;; data-image : -> Image
    ;; RETURNS: the actual data representatio of this controller
    ;; Strategy: abstract method
    (abstract data-image)
    ;===========================================================================
    ;; handler-image : -> Image
    ;; RETURNS: the image of the handler
    ;; Strategy: combine simpler functions
    (define (handler-image)
      (square HANDLER-SIDE "outline"
              (if handler-selected? "red" "black")))
    ;===========================================================================
    ;; handler-x : -> Int
    ;; handler-y : -> Int
    ;; RETURNS: the x/y position of the handler
    ;; Strategy: combine simpler functions
    (define (handler-x) (+ (- x (/ width 2)) HALF-HANDLER-SIDE))
    (define (handler-y) (+ (- y (/ height 2)) HALF-HANDLER-SIDE))
    ;===========================================================================
    ;; in-controller? : Int Int -> Boolean
    ;; RETURNS: true iff the given mouse position is inside this controller
    ;; Strategy: combine simpler functions
    (define (in-controller? mx my)
      (and (<= (abs (- mx x)) (/ width 2))
           (<= (abs (- my y)) (/ height 2))))
    ;===========================================================================
    ;; in-handler? : Int Int -> Boolean
    ;; RETURNS: true iff the given mouse position in inside this controller's
    ;;          handler
    ;; Strategy: combine simpler functions
    (define (in-handler? mx my)
      (and (<= (abs (- mx (handler-x))) HALF-HANDLER-SIDE)
           (<= (abs (- my (handler-y))) HALF-HANDLER-SIDE)))
    ;===========================================================================
    ;; after-button-down : Int Int -> 
    ;; GIVEN: mouse position of button down event
    ;; EFFECT: update this controller according to the given button down
    ;;         event mouse position
    ;; Strategy: set the controller-selected? to true iff mx,my is inside
    ;;           this controlle; set the handler-selected? to true iff
    ;;           mx,my is inside this controller's handler; save mouse position;
    ;;           update ticking in model if needed
    (define/public (after-button-down mx my)
      (set! controller-selected? (in-controller? mx my))
      (set! handler-selected? (in-handler? mx my))
      (set! saved-mx mx)
      (set! saved-my my)
      (update-ticking))
    ;===========================================================================
    ;; update-ticking : -> Void
    ;; EFFECT: update the model to stop/start ticking appropriately
    ;; Strategy: abstract method
    (abstract update-ticking)
    ;===========================================================================
    ;; after-drag : Int Int -> Void
    ;; EFFECT: update this controller according to the given drag mouse location
    ;; Strategy: if handler is selected: update x,y to follow the given
    ;;           mouse location, and store mouse location
    ;;           if controller is selected,
    ;;           pass the drag event to data-after-drag method
    (define/public (after-drag mx my)
      (cond
        [handler-selected?
          (begin
            (set! x (+ x (- mx saved-mx)))
            (set! y (+ y (- my saved-my)))
            (set! saved-mx mx)
            (set! saved-my my))]
        [controller-selected? (data-after-drag mx my)]
        [else 'nothing]))
    ;===========================================================================
    ;; data-after-drag : -> Void
    ;; EFFECT: update this controller according to the drag event
    ;; Strategy: abstract method
    (abstract data-after-drag)
    ;===========================================================================
    ;; after-button-up : Int Int -> Void
    ;; GIVEN: mouse up event location
    ;; EFFECT: update this controller according to the mouse up event
    ;; Strategy: set handler-selected to false and update model to start ticking
    (define/public (after-button-up mx my)
      (set! handler-selected? false)
      (send model update (make-tick-indicator true)))
    ;===========================================================================
    ;; after-key-event : KeyEvent -> Void
    ;; EFFECT: update this controller according to the givne keyevent
    ;; Strategy: abstract method
    (abstract after-key-event)
    ;===========================================================================
    ;; set-particle-x : Int -> Void
    ;; EFFECT: update cache and model particle by changing x to given value
    ;; Strategy: combine simpler methods
    (define/public (set-particle-x new-x)
      (update-x (keep-in-bound 0 FIELD-WIDTH new-x))
      (send this update-model))
    ;===========================================================================
    ;; set-particle-y : Int -> Void
    ;; EFFECT: update cache and model particle by changing y to given value
    ;; Strategy: combine simpler methods
    (define/public (set-particle-y new-y)
      (update-y (keep-in-bound 0 FIELD-HEIGHT new-y))
      (send this update-model))
    ;===========================================================================
    ;; set-particle-vx : Int -> Void
    ;; EFFECT: update cache and model particle by changing vx to given value
    ;; Strategy: combine simpler methods
    (define/public (set-particle-vx new-vx)
      (update-vx new-vx)
      (send this update-model))
    ;===========================================================================
    ;; set-particle-vy : Int -> Void
    ;; EFFECT: update cache and model particle by changing vy to given value
    ;; Strategy: combine simpler methods
    (define/public (set-particle-vy new-vy)
      (update-vy new-vy)
      (send this update-model))
    ;===========================================================================
    ;; update-x : Int -> Void
    ;; EFFECT: update cache particle by changing x to given value
    ;; Strategy: use constructor template for Particle 
    (define (update-x new-x)
      (set!
       particle
       (make-particle
        new-x
        (particle-y particle)
        (particle-vx particle)
        (particle-vy particle))))
    ;===========================================================================
    ;; update-y : Int -> Void
    ;; EFFECT: update cache particle by changing y to given value
    ;; Strategy: use constructor template for Particle 
    (define (update-y new-y)
      (set!
       particle
       (make-particle
        (particle-x particle)
        new-y
        (particle-vx particle)
        (particle-vy particle))))
    ;===========================================================================
    ;; update-vx : Int -> Void
    ;; EFFECT: update cache particle by changing vx to given value
    ;; Strategy: use constructor template for Particle 
    (define (update-vx new-vx)
      (set!
       particle
       (make-particle
        (particle-x particle)
        (particle-y particle)
        new-vx
        (particle-vy particle))))
    ;===========================================================================
    ;; update-vy : Int -> Void
    ;; EFFECT: update cache particle by changing vy to given value
    ;; Strategy: use constructor template for Particle 
     (define (update-vy new-vy)
      (set!
       particle
       (make-particle
        (particle-x particle)
        (particle-y particle)
        (particle-vx particle)
        new-vy)))
    ;===========================================================================
    ;; keep-in-bound : Int Int Int -> Int
    ;; GIVEN: range lo & hi , and a value
    ;; WHERE: lo <= hi
    ;; RETURNS: an in-bound value that is cloest to the given value
    ;; Strategy: case on whether value higher than hi or lower than lo
    (define (keep-in-bound lo hi value)
      (cond
        [(<= value lo) (+ lo 1)]
        [(>= value hi) (- hi 1)]
        [else value]))
    ;===========================================================================
    ;; A controller does not respond to move and tick
    (define/public (after-move mx my) 'nothing)
    (define/public (after-tick) 'nothing)
    ;===========================================================================
    ;; test methods
    ;; Strategy: return a field
    (define/public (for-test:get-particle) particle)
    (define/public (for-test:get-model) model)
    (define/public (for-test:get-x) x)
    (define/public (for-test:get-y) y)
    (define/public (for-test:handler-selected?) handler-selected? )
    (define/public (for-test:controller-selected?) controller-selected?)
    ;===========================================================================
    ))
;;==============================================================================
;; TESTS see tests.rkt
