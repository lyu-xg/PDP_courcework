#lang racket
(require "extras.rkt")
(require rackunit)
(require "interfaces.rkt")
(require "PositionController.rkt")
(require "VelocityController.rkt")
(require "GeneralController.rkt")
(require "XYController.rkt")
(require "model.rkt")
;===============================================================================
(define STATISTICS-WIDTH 150)
(define STATISTICS-HEIGHT 50)
(define LEFT-ARROW-EVENT "left")
(define RIGHT-ARROW-EVENT "right")
(define UP-ARROW-EVENT "up")
(define DOWN-ARROW-EVENT "down")
;===============================================================================
(begin-for-test
  (local ((define m (make-model))
          (define pc (new PositionController% [model m]))
          (define vc (new VelocityController% [model m]))
          (define xyc (new XYController% [model m]))
          )
    ;; For PositionController
    (send m register pc)
    (send pc set-particle-x 10)
    (check-equal? (particle-x (send m for-test:get-particle)) 10
                  "after setting x=10 in c, m should also be updated")
    (send pc set-particle-y 20)
    (check-equal? (particle-y (send m for-test:get-particle)) 20
                  "after setting y=20 in c, m should also be updated")
    (send pc set-particle-vx 5)
    (check-equal? (particle-vx (send m for-test:get-particle)) 5
                  "after setting vx=5 in c, m should also be updated")
    (send pc set-particle-vy 15)
    (check-equal? (particle-vy (send m for-test:get-particle)) 15
                  "after setting vy=15 in c, m should also be updated")
    (send pc set-particle-x -10)
    (check-equal? (particle-x (send m for-test:get-particle)) 1
                  "when try to set x to negative, x should be bounded")
    (send pc set-particle-y -20)
    (check-equal? (particle-y (send m for-test:get-particle)) 1
                  "when try to set y to negative, y should be bounded")
    (send pc after-button-down INIT-X INIT-Y)
    (check-true
     (send pc for-test:controller-selected?)
     "when button down on the controller, controller should be selected")
    (send pc increment-x-event)
    (check-equal? (particle-x (send m for-test:get-particle)) (+ 1 5)
                  "after increment-x-event, x should be incremented")
    (send pc increment-y-event)
    (check-equal? (particle-y (send m for-test:get-particle)) (+ 1 5)
                  "after increment-y-event, y should be incremented")
    (send pc decrement-x-event)
    (check-equal? (particle-x (send m for-test:get-particle)) 1
                  "after decrement-x-event, x should be decremented")
    (send pc decrement-y-event)
    (check-equal? (particle-y (send m for-test:get-particle)) 1
                  "after decrement-y-event, y should be decremented")

    ;; For VelocityController
    (send m register vc)
    (send vc set-particle-x 10)
    (check-equal? (particle-x (send m for-test:get-particle)) 10
                  "after setting x=10 in c, m should also be updated")
    (send vc set-particle-y 20)
    (check-equal? (particle-y (send m for-test:get-particle)) 20
                  "after setting y=20 in c, m should also be updated")
    (send vc set-particle-vx 5)
    (check-equal? (particle-vx (send m for-test:get-particle)) 5
                  "after setting vx=5 in c, m should also be updated")
    (send vc set-particle-vy 15)
    (check-equal? (particle-vy (send m for-test:get-particle)) 15
                  "after setting vy=15 in c, m should also be updated")
    (send vc increment-x-event)
    (check-equal? (particle-vx (send m for-test:get-particle)) 10
                  "after increment-x-event, x should be incremented")
    (send vc increment-y-event)
    (check-equal? (particle-vy (send m for-test:get-particle)) 20
                  "after increment-y-event, y should be incremented")
    (send vc decrement-x-event)
    (check-equal? (particle-vx (send m for-test:get-particle)) 5
                  "after decrement-x-event, x should be decremented")
    (send vc decrement-y-event)
    (check-equal? (particle-vy (send m for-test:get-particle)) 15
                  "after decrement-y-event, y should be decremented")

    ;; For StatisticController
    (send vc after-button-down INIT-X INIT-Y)
    (send vc after-key-event RIGHT-ARROW-EVENT)
    (check-equal? (particle-vx (send m for-test:get-particle)) 10
                  "after increment-x-event, x should be incremented")
    (send vc after-key-event DOWN-ARROW-EVENT)
    (check-equal? (particle-vy (send m for-test:get-particle)) 20
                  "after increment-y-event, y should be incremented")
    (send vc after-key-event LEFT-ARROW-EVENT)
    (check-equal? (particle-vx (send m for-test:get-particle)) 5
                  "after decrement-x-event, x should be decremented")
    (send vc after-key-event UP-ARROW-EVENT)
    (check-equal? (particle-vy (send m for-test:get-particle)) 15
                  "after decrement-y-event, y should be decremented")
    (send vc update-ticking)
    (check-true (send m for-test:ticking?)
                "StatisticController should not effect model's ticking")

    ;; For VisualizedController
    (send xyc after-button-down INIT-X INIT-Y)
    (check-false
     (send m for-test:ticking?)
     "VisualizedController should stop model from ticking after button down")
    ))