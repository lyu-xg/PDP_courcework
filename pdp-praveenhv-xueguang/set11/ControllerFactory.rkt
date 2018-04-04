#lang racket
(require "extras.rkt")
(require rackunit)
(require "Interfaces.rkt")
(require "XYController.rkt")
(require "XController.rkt")
(require "YController.rkt")
(require "PositionController.rkt")
(require "VelocityController.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(require "WidgetWorks.rkt")
;===============================================================================
(provide make-controller-factory)
;===============================================================================
;; CONSTANTS
(define NEW-XYC-EVENT "z")
(define NEW-X-EVENT "x")
(define NEW-Y-EVENT "y")
(define NEW-P-EVENT "p")
(define NEW-V-EVENT "v")
;===============================================================================
;; A ControllerClass is one of
;; -- XYController%
;; -- YController%
;; -- XController%
;; -- PositionController%
;; -- VelocityController%
;===============================================================================
(define (make-controller-factory c m)
  (new ControllerFactory% [w c] [m m]))
;===============================================================================
(define ControllerFactory%
  (class* object% (SWidget<%>)
    ;===========================================================================
    ; the world in which the controllers will live
    (init-field w)   ; World<%>
    ;===========================================================================
    ; the model to which the controllers will be connected
    (init-field m)   ; Model<%>
    ;===========================================================================
    (super-new)
    ;===========================================================================
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a keyevent
    ;; EFFECT: create new controllers based on key event
    ;; Strategy: case on the key event given
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-XYC-EVENT) (add-viewer XYController%)]
        [(key=? kev NEW-P-EVENT) (add-viewer PositionController%)]
        [(key=? kev NEW-V-EVENT) (add-viewer VelocityController%)]
        [(key=? kev NEW-X-EVENT) (add-viewer XController%)]
        [(key=? kev NEW-Y-EVENT) (add-viewer YController%)]
        [else 'DoNothing]
        ))
    ;===========================================================================
    ;; add-viewer : ControllerClass -> Void
    ;; GIVEN: a controller class to instanciate
    ;; EFFECT: instanciate a controller and register the controller to model
    ;; Strategy: combine simpler function
    (define/public (add-viewer viewer-class)
      (let ([new-view (new viewer-class [model m])])
        (send w add-stateful-widget new-view)
        (send m register new-view)))
    ;===========================================================================
    ;; -> Void
    ;; Controller factory does not response to anything other than key event
    ;; Strategy: do nothing and return Void
    (define/public (after-button-down mx my) 'trap)
    (define/public (after-button-up mx my) 'trap)
    (define/public (after-drag mx my) 'trap)
    (define/public (after-move mx my) 'trap)
    (define/public (after-tick) 'trap)
    (define/public (add-to-scene s) s)

    ))
;===============================================================================
(require "model.rkt")
;===============================================================================
;; TESTS
(begin-for-test
  (local ((define w (container-init 500 600))
          (define m (make-model))
          (define cf1 (make-controller-factory w m))
          )
    (send cf1 after-key-event NEW-XYC-EVENT)
    (check-equal? (length (send m for-test:get-controllers)) 1
                  "after new-xyc-event, a new controller should be in model")
    (send cf1 after-key-event NEW-V-EVENT)
    (send cf1 after-key-event NEW-P-EVENT)
    (send cf1 after-key-event NEW-X-EVENT)
    (send cf1 after-key-event NEW-Y-EVENT)
    (check-equal? (length (send m for-test:get-controllers)) 5
                  "after other events, there should be five total controllers")
    (send cf1 after-key-event "h")
    (check-equal? (length (send m for-test:get-controllers)) 5
                  "after irrelavent event, there should not be more controller")
    (send cf1 after-button-down 100 200)
    (send cf1 after-button-up 100 200)
    (send cf1 after-drag 100 200)
    (send cf1 after-move 100 200)
    (send cf1 after-tick)
    (check-equal? (length (send m for-test:get-controllers)) 5
                  "after irrelavent event, there should not be more controller")
    (check-equal? (send cf1 add-to-scene (empty-scene 100 200))
                  (empty-scene 100 200)
                  "ControllerFactory should not be painted on canvas")
    

    ))























