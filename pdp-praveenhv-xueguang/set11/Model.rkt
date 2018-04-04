#lang racket

;; The Model consists of a particle and its controllers
;; It accepts commands and reports when its status changes

(require rackunit)
(require "extras.rkt")
(require "interfaces.rkt")
(require "PerfectBounce.rkt")
(require "WidgetWorks.rkt")

(provide make-model)
;===============================================================================
;; make-model: -> Model
;; RETURNS: a new initialzed model object
;; Strategy: use constructor template for Model
(define (make-model) (new Model%))
;===============================================================================
;; Constructor template for Model%:
;; (new Model%)
;; Models will receive signals from the Container, so it implements SWidget<%>
;; Models also publishes particle data to registered controllers

(define Model%
  (class* object% (Model<%> SWidget<%>)
    ;===========================================================================
    ;; boundaries of the particle
    (init-field [boundary (make-rect 0 FIELD-WIDTH 0 FIELD-HEIGHT)])
    ;; (data type Rect is defined in the given PerfectBounce.rkt)
    ;===========================================================================
    ;; position and velocity of the object
    (init-field [particle (make-particle PARTICLE-INIT-X PARTICLE-INIT-Y 0 0)])
    ;; (data type Particle is defined in the given PerfectBounce.rkt)
    ;===========================================================================
    ; ListOfController.  The list of registered controllers
    (init-field [controllers empty])
    ;===========================================================================
    ;; Boolean value, representing whether the model should be ticking
    (init-field [ticking? true])
    ;===========================================================================
    (super-new)
    ;===========================================================================
    ;; after-tick : -> Void
    ;; EFFECT: moves the particle by vx, vy and publishes particle
    ;; Strategy: combine simpler functions
    (define/public (after-tick)
      (if ticking?
          (begin (set! particle (particle-after-tick particle boundary))
                 (publish-particle))
          'nothing))
    ;===========================================================================
    ;; register: Controller -> Void
    ;; EFFECT: register the new controller and send it current particle data
    ;; Strategy: combine simpler functions
    (define/public (register c)
      (set! controllers (cons c controllers))
      (send c receive-signal particle))
    ;===========================================================================
    ;; update: Signal -> Void
    ;; EFFECT: update this model according to the given signal
    ;; Strategy: case on the signal recieved
    (define/public (update signal)
      (cond
        [(particle? signal) (update-particle signal)]
        [(tick-indicator? signal)
         (set! ticking? (tick-indicator-ticking? signal))]
        [else 'nothing]))
    ;===========================================================================
    ;; update-particle : Particle -> Void
    ;; EFFECT: update this model according to the given particle
    ;;         and publish the updated particle if it has any change
    ;; Strategy: Case on whether the given particle matches current particle
    ;;           if given particle is the same as current particle, do nothing
    ;;           else update the particle and publish it to all controllers
    (define (update-particle p)
      (let ([current-p particle])
        (set! particle p)
        (if (not (equal? current-p p))
            (publish-particle)
            'nothing)))
    ;===========================================================================
    ;; publish-particle : -> Void
    ;; EFFECT: publish particle to each controller
    ;; Strategy: use HOF for-each on controllers
    (define (publish-particle)
      (for-each
       ;; Controller -> Void
       (lambda (controller)
         (send controller receive-signal particle))
       controllers))
    ;===========================================================================
    ;; The model responds to after-tick, but not to any of the other
    ;; SWidget messages
    (define/public (after-button-down mx my) 'trap)
    (define/public (after-button-up mx my) 'trap)
    (define/public (after-drag mx my) 'trap)
    (define/public (after-move mx my) 'trap)
    (define/public (after-key-event kev) 'trap)
    (define/public (add-to-scene s) s)
    ;===========================================================================
    ;; Test methods
    ;; Strategy: return a field
    (define/public (for-test:get-particle) particle)
    (define/public (for-test:get-controllers) controllers)
    (define/public (for-test:ticking?) ticking?)
    ))

;===============================================================================

(require "PositionController.rkt")
(require 2htdp/image)
(require 2htdp/universe)

(begin-for-test
  (local ((define m1 (make-model))
          (define p (make-particle 10 20 1 0))
          (define c (new PositionController% [model m1]))
          )
    (check-equal? (length (send m1 for-test:get-controllers)) 0
                  "there should be no controller in a new model")
    (send m1 register c)
    (check-equal? (length (send m1 for-test:get-controllers)) 1
                  "after registration, the model now should have 1 controller")
    (check-equal? (particle-x (send m1 for-test:get-particle)) PARTICLE-INIT-X
                  "the particle x should be at PARTICLE-INIT-X")
    (send m1 after-tick)
    (check-equal? (particle-x (send m1 for-test:get-particle)) PARTICLE-INIT-X
                  "particle should remain still after tick when no velocity")
    (send m1 update p)
    (send m1 update p)
    (send m1 after-tick)
    (check-equal? (particle-x (send m1 for-test:get-particle)) (+ 10 1)
                  "particle should be moving when has velocity")
    (send m1 update (make-tick-indicator false))
    (send m1 after-tick)
    (check-equal? (particle-x (send m1 for-test:get-particle)) (+ 10 1)
                  "after recieve stop ticking update, particle should stay still")
    
    (send m1 update "something")
    (check-equal? (particle-x (send m1 for-test:get-particle)) (+ 10 1)
                  "given irrelavent update, particle won't change")
    (send m1 after-button-down 100 200)
    (check-equal? (send m1 for-test:get-particle) (make-particle 11 20 1 0)
                  "after-button-down should not change anything in particle")
    (send m1 after-button-up 100 200)
    (check-equal? (send m1 for-test:get-particle) (make-particle 11 20 1 0)
                  "after-button-up should not change anything in particle")
    (send m1 after-drag 100 200)
    (check-equal? (send m1 for-test:get-particle) (make-particle 11 20 1 0)
                  "after-button-drag should not change anything in particle")
    (send m1 after-move 100 200)
    (check-equal? (send m1 for-test:get-particle) (make-particle 11 20 1 0)
                  "after-button-move should not change anything in particle")
    (send m1 after-key-event "s")
    (check-equal? (send m1 for-test:get-particle) (make-particle 11 20 1 0)
                  "after-key-event should not change anything in particle")
    (check-equal? (send m1 add-to-scene (empty-scene 160 90))
                  (empty-scene 160 90)
                  "model should not be painted on canvas")
    ))


    
  




    

    
