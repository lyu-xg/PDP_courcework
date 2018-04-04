#lang racket
(require "GeneralController.rkt")
(require "extras.rkt")
(require "interfaces.rkt")
(require 2htdp/image)
(require 2htdp/universe)
;===============================================================================
(provide StatisticController%)
;===============================================================================
(define STATISTICS-WIDTH 150)
(define STATISTICS-HEIGHT 50)
(define LEFT-ARROW-EVENT "left")
(define RIGHT-ARROW-EVENT "right")
(define UP-ARROW-EVENT "up")
(define DOWN-ARROW-EVENT "down")
;===============================================================================
;;StatisticController is used to make changes to the attributes of the particle
;;It extends the general controller. THis controller handles the
;;key inputs and performs actions based on the inputs
(define StatisticController%
  ;; StatisticController class is the super class for VelocityController
  ;; and PositionController 
  (class* GeneralController% (Controller<%> GeneralControllerHooks<%>)
    ;===========================================================================
    ;;Some fields are inherited from parent class to perform action on them
    ;;or use the attributes 
    ;;to make chnages on other fields
    (inherit-field model particle controller-selected?)
    ;; INTERP: see GeneralController.rkt
    ;===========================================================================
    (super-new [width STATISTICS-WIDTH]    ;; Int
               [height STATISTICS-HEIGHT]) ;; Int
    ;; the width and height of this controller on the canvas
    
    ;===========================================================================
    ;; after-key-event : KeyEvent -> Void
    ;;GIVEN : KeyEvent
    ;;RETURNS : Void
    ;;EFFECT: Increments or decrements some attributes of the particle.
    ;;EXAMPLE :Refer Test Cases
    ;;STRATEGY: Cases on keyEvents.	
   
    ;; update the cache particle and update the model

    ;; after-key-event : KeyEvent -> Void
    ;; EFFECT : update the cache particle and update the model according to the
    ;;          keyevent
    ;; Strategy: case on whether this controller is selected

    (define/override (after-key-event kev)
      (if controller-selected?
          (dispatch-events kev)
          'nothing))

    ;;===========================================================================
    ;; dispatch-events : KeyEvent -> Void
    ;;GIVEN : KeyEvent
    ;;RETURNS : Void
    ;;EFFECT: Particle attributes are updated based on the key inputs.
    ;;EXAMPLE :Refer Test Case
    ;;STRATEGY:Cases on keyEvents.	
    ;; dispatch-events : KeyEvent -> Void
    ;; dispatch keyevents to update particle attributes and update the model

    (define (dispatch-events kev)
      (cond
        [(key=? kev LEFT-ARROW-EVENT)
         (decrement-x-event)]
        [(key=? kev DOWN-ARROW-EVENT)
         (increment-y-event)]
        [(key=? kev RIGHT-ARROW-EVENT)
         (increment-x-event)]
        [(key=? kev UP-ARROW-EVENT)
         (decrement-y-event)]
        [else 'nothing])
      )
    ;;==========================================================================
    ;;update-ticking: -> Void
    ;;GIVEN : No Input
    ;;RETURNS : Void
    ;;EFFECT: paticle doesnt respond to update-ticking.
    ;;STRATEGY:Return Void	
    (define/override (update-ticking) 'nothing)
    ;===========================================================================
    ;;data-after-drag:PosInt PosInt-> Void
    ;;GIVEN : Mouse Location
    ;;RETURNS : Void
    ;;EFFECT: No Effect as the controller doesnt respond to after drag
    ;;EXAMPLE :Refer Test Cases
    ;;STRATEGY: Return Void 	 
    (define/override (data-after-drag mx my) 'nothing)
    ;===========================================================================
    ;; -> Void
    ;; EFFECT: update this controller according to the event
    ;; Strategy: abstract methods
    (abstract decrement-x-event)
    (abstract increment-x-event)
    (abstract decrement-y-event)
    (abstract increment-y-event)
    ;;==========================================================================
    ))