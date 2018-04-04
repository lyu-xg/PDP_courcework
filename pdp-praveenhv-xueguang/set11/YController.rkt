#lang racket
(require "VisualizedController.rkt")
(require "extras.rkt")
(require "interfaces.rkt")
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(provide YController%)
;===============================================================================
(define XYC-WIDTH 50)
(define XYC-HEIGHT 150)
;===============================================================================
;; Constructor template for YController%:
;; (new YController%)
;; YController will receive signals from the Container. Handels the changes
;;related to Y movement display.
;; YController also reports model updates.
(define YController%
  
  (class* VisualizedController% (Controller<%>)
    ;;==========================================================================
    ;;setting the dimension of the controller boundary. Represents the outer
    ;;rectangle during rendering.
    (super-new [width XYC-WIDTH]
               [height XYC-HEIGHT])
    ;;==========================================================================
    ;;Inherits some of the fields from the general controller to be used in this
    ;;controller.
    (inherit-field particle controller-selected? saved-mx saved-my)
    ;;==========================================================================
    ;;data-image: No Inputs -> Image
    ;;Returns : Image having the particle and the controller boundary represented
    ;;by a blue rectangle.
    ;;Strategy : Combine Simpler Functions.
    (define/override (data-image)
      (local ((define data-canvas
                (rectangle XYC-WIDTH FIELD-HEIGHT "solid" "white"))
              (define rectangle-blue
                (rectangle XYC-WIDTH FIELD-HEIGHT "outline" "blue"))
              (define particle-image
                (overlay (circle 3 "solid" "black") (circle 10 "solid" "red"))))
        (overlay rectangle-blue
                 (place-image particle-image 
                              (/ XYC-WIDTH 2)
                              (particle-y particle)
                              data-canvas))))
    ;;==========================================================================
    ;;data-after-drag:PosInt PosInt -> Void
    ;;GIVEN : Mouse locations
    ;;RETURNS :Void
    ;;EFFECT: updates the particle based on the mouse location and whether the
    ;;        mouse position is inside the controller.
    ;;EXAMPLE :Refer Test Cases.
    ;;STRATEGY: Use set to update the attributes of the particle.	
    (define/override (data-after-drag mx my)
      (if controller-selected?
          (begin
            (send this set-particle-y (+ (particle-y particle) (- my saved-my)))
            (set! saved-mx mx)
            (set! saved-my my)
            )
          'nothing
          ))
    ))
;;==============================================================================
(require "Model.rkt")
(begin-for-test
  (local (  (define m1  (make-model))
            (define p1 (make-particle 10 20 1 0))
            (define c1 (new YController% [model m1]))
            
            (define m2  (make-model))
            (define p2 (make-particle 10 20 1 0))
            (define c2 (new YController% [model m1]))
            )
    (send m1 register c1)
    (send m2 register c2)
    (check-equal?
     (send c1 add-to-scene (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
     (send c2 add-to-scene (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
     "Both controllers are same")
    (send c1 after-button-down 282 177)
    (check-equal?
     (send c1 for-test:handler-selected?  ) true
     "handler selected" )
    (send c1 after-button-up 150 150)
    (check-equal?
     (send c1 for-test:handler-selected?  ) false
     "handler deselected" )
    (send c1 after-button-down 300 250)
    (check-equal?
     (send c1 for-test:controller-selected?  ) true
     "controller selected" )
    (send c1 after-button-up 300 250)
    (check-equal?
     (send c1 for-test:controller-selected?  ) true
     "controller deselected" )
    (send c1 data-after-drag 301 251)
    
    (check-equal? (particle-y (send c1 for-test:get-particle)) 51
                  "moved x by 1")
    (send c1 after-button-down 0 0)
    (check-equal?
     (send c1 for-test:controller-selected?  ) false
     "controller deselected" )
    (send c1 data-after-drag 301 251)        
    ))





