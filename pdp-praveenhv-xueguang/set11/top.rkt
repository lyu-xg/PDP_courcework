#lang racket

(require "WidgetWorks.rkt")
(require "Interfaces.rkt")
(require "Model.rkt")
(require "ControllerFactory.rkt")
(require "tests.rkt")
;===============================================================================
;; run with (run 0.2)
;===============================================================================
;; run : Number -> Container
;; create a container, install a factory, and run.
;; run : PosReal -> Void
;; GIVEN: a frame rate, in sec/tick
;; EFFECT: Creates and runs the MVC simulation with the given frame rate.
;; Strategy: combine simpler functions
(define (run rate)
  (let* ([c (container-init CANVAS-WIDTH CANVAS-HEIGHT)]
         [m (make-model)])
      (send c add-stateful-widget m)
      (send c add-stateful-widget (make-controller-factory c m))
      (send c run rate)))
