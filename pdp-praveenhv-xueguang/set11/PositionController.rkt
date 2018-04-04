#lang racket
(require "StatisticController.rkt")
(require "interfaces.rkt")
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)
;===============================================================================
(provide PositionController%)
;===============================================================================
(define INC-AMOUNT 5)

;;=============================================================================
;;PositionController is used to make changes to the position of the particle
;;It extends the Statistic and general controllers. THis controller handles the
;;positioning of the particle and updates the model about
;;the changes due to inputs

;===============================================================================

(define PositionController%
  ;; A VelocityController displays statistics and controls velocity
  (class* StatisticController%
    ;===========================================================================
    (Controller<%>
     GeneralControllerHooks<%>
     StatisticControllerHooks<%>)

    ;;Some fields are inherited from parent class to perform action on them
    ;;or use the attributes 
    ;;to make chnages on other fields

    ;; PositionController should implement Controller, GeneralControllerHooks,
    ;; and StatisticsControllerHooks
    ;===========================================================================
    (inherit-field particle controller-selected?)
    ;; INTERP: see GeneralController.rkt
    ;===========================================================================
    (super-new)
    ;;==========================================================================
    ;;increment-x-event: -> Void
    ;;GIVEN : No Inputs
    ;;RETURNS : Void
    ;;EFFECT: Sets the particle x co-ordinate to a new value after increment
    ;;EXAMPLE : Refer Test Cases
    ;;STRATEGY: Call a parent class function using this.	  
    (define/override (increment-x-event)
      (send this set-particle-x (+ (particle-x particle) INC-AMOUNT)))
    ;;==========================================================================
    ;;increment-y-event: -> Void
    ;;GIVEN :No Inputs
    ;;RETURNS :Void
    ;;EFFECT: Sets the particle y co-ordinate to a new value after increment
    ;;EXAMPLE : Refer Test Cases
    ;;STRATEGY:Call a parent class function using this.	  
    (define/override (increment-y-event)
      (send this set-particle-y (+ (particle-y particle) INC-AMOUNT)))
    ;;==========================================================================
    ;; decrement-x-event:  ->Void
    ;;GIVEN :No Inputs
    ;;RETURNS :Void
    ;;EFFECT:  Sets the particle x co-ordinate to a new value after decrement
    ;;EXAMPLE : Refer Test Cases
    ;;STRATEGY:Call a parent class function using this.	  	 
    (define/override (decrement-x-event)
      (send this set-particle-x (- (particle-x particle) INC-AMOUNT)))
    ;;==========================================================================
    ;;decrement-y-event:  ->Void
    ;;GIVEN :No Inputs
    ;;RETURNS :Void
    ;;EFFECT:Sets the particle y co-ordinate to a new value after decrement
    ;;EXAMPLE : Refer Test Cases
    ;;STRATEGY:Call a parent class function using this.	  	 
    (define/override (decrement-y-event)
      (send this set-particle-y (- (particle-y particle) INC-AMOUNT)))
    ;;==========================================================================
    ;; data-image : -> Image
    ;; GIVEN :No Inputs
    ;; RETURNS : a Image that represents the central portion of this controller
    ;; EXAMPLE : Refer Test Cases
    ;; STRATEGY:Combine Simpler functions.	  	 
    (define/override (data-image)
      (let ([color (if controller-selected? "red" "black")])
        (above
         (text "Arrow keys change position" 10 color)
         (text (string-append
                "X = " (number->string (particle-x particle))
                " Y = " (number->string (particle-y particle)))
               12 color)
         (text (string-append
                "VX = " (number->string (particle-vx particle))
                " VY = " (number->string (particle-vy particle)))
               12 color))))
    ))
;;==============================================================================
;; TESTS: see tests.rkt