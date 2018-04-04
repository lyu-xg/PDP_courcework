#lang racket
(require "StatisticController.rkt")
(require "interfaces.rkt")
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)
;===============================================================================
(provide VelocityController%)
;===============================================================================
(define INC-AMOUNT 5)
;;=============================================================================
;;VelocityController is used to make changes to the velocity of the particle
;;It extends the Statistic and general controllers. THis controller handles the
;;velocity of the particle and updates the model about the changes due to inputs.
;===============================================================================

(define VelocityController%
  ;; A VelocityController displays statistics and controls velocity
  (class* StatisticController%
    ;===========================================================================
    (Controller<%>
     GeneralControllerHooks<%>
     StatisticControllerHooks<%>)

    ;;Some fields are inherited from parent class to perform action on them
    ;;or use the attributes 
    ;;to make chnages on other fields

    ;; VelocityController should implement Controller, GeneralControllerHooks,
    ;; and StatisticsControllerHooks
    ;===========================================================================
    (inherit-field particle controller-selected?)
    ;; INTERP: see GeneralController.rkt
    ;===========================================================================
    (super-new)
    ;;==========================================================================	
    ;;increment-x-event: -> Void
    ;;GIVEN : No INputs
    ;;RETURNS : Void
    ;;EFFECT: Sets the particles x direction velocity to a new value
    ;;        after increment
    ;;EXAMPLE : see Test Cases below
    ;;STRATEGY:Call a parent class function using this.	 
    (define/override (increment-x-event)
      (send this set-particle-vx (+ (particle-vx particle) INC-AMOUNT)))
    ;;==========================================================================
    ;;increment-y-event: -> Void
    ;;GIVEN : No Inputs
    ;;RETURNS : Void
    ;;EFFECT: Sets the particles y direction velocity to a new value
    ;;        after increment
    ;;EXAMPLE : see Test Cases below
    ;;STRATEGY:Call a parent class function using this.	 
    (define/override (increment-y-event)
      (send this set-particle-vy (+ (particle-vy particle) INC-AMOUNT)))
    ;;==========================================================================
    ;;decrement-x-event: -> Void
    ;;GIVEN : No Inputs
    ;;RETURNS : Void
    ;;EFFECT: Sets the particles x direction velocity to a new value
    ;;        after decrement
    ;;EXAMPLE : see Test Cases below
    ;;STRATEGY:Call a parent class function using this.	 
    (define/override (decrement-x-event)
      (send this set-particle-vx (- (particle-vx particle) INC-AMOUNT)))
    ;;==========================================================================
    ;;decrement-y-event: -> Void
    ;;GIVEN : No Inputs
    ;;RETURNS : Void
    ;;EFFECT: Sets the particles y direction velocity to a new value
    ;;        after decrement
    ;;EXAMPLE :
    ;;STRATEGY:Call a parent class function using this.	 
    (define/override (decrement-y-event)
      (send this set-particle-vy (- (particle-vy particle) INC-AMOUNT)))
    ;;==========================================================================
    ;;data-image:-> Image
    ;;GIVEN :No Inputs
    ;;RETURNS : an image that represent the central portion of this controller
    ;;EXAMPLE : see Test Cases below
    ;;STRATEGY:Combine Simpler functions
    (define/override (data-image)
      (let ([color (if controller-selected? "red" "black")])
        (above
         (text "Arrow keys change velocity" 10 color)
         (text (string-append
                "X = " (number->string (particle-x particle))
                " Y = " (number->string (particle-y particle)))
               12 color)
         (text (string-append
                "VX = " (number->string (particle-vx particle))
                " VY = " (number->string (particle-vy particle)))
               12 color))))
    
    ))