#lang racket

;; The Model consists of a particle and its controllers
;; It accepts commands and reports when its status changes

(require rackunit)
(require "extras.rkt")
(require "interfaces.rkt")
(require "PerfectBounce.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(provide make-model)

;; CONSTANTS
(define FIELD-WIDTH 150)
(define FIELD-HEIGHT 100)
(define INIT-X (/ FIELD-WIDTH 2))
(define INIT-Y (/ FIELD-HEIGHT 2))


;; make-model: -> Model
(define (make-model) (new Model%))

;; Constructor template for Model%:
;; (new Model%)

(define Model%
  (class* object% (Model<%>)

    ;; boundaries of the particle
    (field [boundary (make-rect 0 FIELD-WIDTH 0 FIELD-HEIGHT)])
    ;; data type Rect is defined in the given PerfectBounce.rkt
    
    ;; position and velocity of the object
    (field [particle (make-particle INIT-X INIT-Y 0 0)])
    ;; data type Particle is defined in the given PerfectBounce.rkt

    (field [x INIT-X] [y INIT-Y])
    (field [vx 0] [vy 0])

    ; ListOfController.  The list of registered controllers
    (init-field [controllers empty])   

    (super-new)

    ;; -> Void
    ;; moves the object by v.
    ;; limits the resulting x to [0, 200].
    ;; publishes particle at every tick
    (define/public (after-tick)
      (set! particle (particle-after-tick particle boundary))
      ;(set! particle (make-particle x y 0 0))
     ; (publish-particle))
      (publish-position))
 
    ;; Controller -> Void
    ;; register the new controller and send it some data
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal particle)))

    ;; Particle -> Void
    ;; update this model according to the given particle
    (define/public (update p)
      (set! particle p)
      (publish-particle))

    ;; report position or velocity to each controller:

    ;; publish-particle : -> Void
    (define (publish-particle)
      (for-each
       ;; Controller -> Void
       (lambda (controller)
         (send controller receive-signal particle))
       controllers))

    (define/public (execute-command cmd)
      (cond
        [(set-position? cmd)
         (begin
           (set! x (set-position-x cmd))
           (set! y (set-position-y cmd))
           (publish-position))]
        [(incr-velocity? cmd)
         (begin
           (set! vx (+ vx (incr-velocity-vx cmd)))
            (set! vy (+ vy (incr-velocity-vy cmd)))
           (publish-velocity))]))


    (define (publish-position)
      (let ((msg (make-report-position x y)))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)
        ))

    (define (publish-velocity)
      (let ((msg (make-report-velocity vx vy)))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)))

    

    ;; The model responds to after-tick, but not to any of the other
    ;; SWidget messages
    (define/public (after-button-down mx my) this)
    (define/public (after-button-up mx my) this)
    (define/public (after-drag mx my) this)
    (define/public (after-move mx my) this)
    (define/public (after-key-event kev) this)
    (define/public (add-to-scene s) s)
    ))






    
  




    

    
