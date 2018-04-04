#lang racket

;; interfaces for MVC example

(require "WidgetWorks.rkt")
(require "PerfectBounce.rkt")

(provide Controller<%> Model<%>
         StatisticControllerHooks<%>
         GeneralControllerHooks<%>)

;; struct for model command language
(provide 
  (struct-out particle)
  (struct-out tick-indicator))

(define FIELD-WIDTH 150)
(define FIELD-HEIGHT 100)
(define PARTICLE-INIT-X (/ FIELD-WIDTH 2))
(define PARTICLE-INIT-Y (/ FIELD-HEIGHT 2))
(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)
(define INIT-X (/ CANVAS-WIDTH 2))
(define INIT-Y (/ CANVAS-HEIGHT 2))

(provide FIELD-WIDTH
         FIELD-HEIGHT
         PARTICLE-INIT-X
         PARTICLE-INIT-Y
         CANVAS-WIDTH
         CANVAS-HEIGHT
         INIT-X
         INIT-Y)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Controller is an object of any class that implements
;; Controller<%>

;; There will be several such classes, and there may be several
;; objects of each such class.

(define Controller<%>    
  (interface (SWidget<%>)

    ;; Signal -> Void
    ;; receive a signal from the model and adjust controller accordingly 
    receive-signal

    ;; -> Void
    ;; update the model based on the current cache of this controller
    update-model
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Model is an object of any class that implements Model<%>.  Models
;; will receive signals from the Container, so they must implement the
;; SWidget<%> interface in order to do so.

(define Model<%>
  (interface (SWidget<%>)

    ;; Controller -> Void
    ;; Registers the given controller to receive signal
    register

    ;; Particle -> Void
    ;; Update this model according to the given Particle
    update
))

;; CONTROLLER/MODEL PROTOCOL:

;; As soon as a controller registers with the model, the model sends
;; the controller a pair of Signals so the controller will know the
;; current state of the model.

;; The controller then sends the model commands, which the model is
;; supposed to execute.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All subclass of StatisticController should implement StatisticControllerHooks

(define StatisticControllerHooks<%>
  (interface ()

    ;; -> Void
    ;; EFFECT: update the cache particle and model
    ;; should be called when x asixs should be decremented
    ;; according to the key event recieved 
    decrement-x-event

    ;; -> Void
    ;; EFFECT: update the cache particle and model
    ;; should be called when x asixs should be incremented
    ;; according to the key event recieved 
    increment-x-event

    ;; -> Void
    ;; EFFECT: update the cache particle and model
    ;; should be called when y asixs should be decremented
    ;; according to the key event recieved 
    decrement-y-event

    ;; -> Void
    ;; EFFECT: update the cache particle and model
    ;; should be called when x asixs should be incremented
    ;; according to the key event recieved 
    increment-y-event))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All subclass of GeneralController should implement GeneralControllerHooks

(define GeneralControllerHooks<%>
  (interface()

    ;; -> Image
    ;; RETURNS: a image that representing the data that
    ;;          this controller should display
    data-image

    ;; Int Int -> Voie
    ;; GIVEN: mouse positions of drag event
    ;; EFFECT: update this controller's cached particle according to drag
    ;;         event location
    data-after-drag

    ;; KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: update this particle and model according to the key event given
    after-key-event

    ;; -> Void
    ;; EFFECT: update wheterh the model should stop/start ticking
    update-ticking))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS FOR COMMUNICATING WITH MODEL

;; A Signal is one of
;; -- Particle
;; -- TickIndicator
;; Template:
#;(define (signal-fn s)
    (cond
      [(particle? s) (particle-fn s)]
      [(tick-indicator? s) (tick-indicator-fn ti)]))


;; A Particle is a (make-particle Int Int Int Int)
;;   the four Int are x, y, vx, vy of the particle respectively
;; Template:
#;(define (particle-fn p)
    (... (particle-x p)
         (particle-y p)
         (particle-vx p)
         (particle-vy p)))

(define-struct tick-indicator (ticking?) #:transparent)
;; A TickIndicator is a (make-tick-indicator Boolean)
;; Tempalte:
#;(define (tick-indicator-fn ti)
    (... (tick-indicator-ticking? ti)))





