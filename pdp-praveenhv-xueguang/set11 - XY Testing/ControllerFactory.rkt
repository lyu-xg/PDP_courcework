#lang racket
(require "Interfaces.rkt")
(require "Controller-XY.rkt")
(require 2htdp/universe)
(require "WidgetWorks.rkt")
(provide ControllerFactory%)



(define ControllerFactory%
  (class* object% (SWidget<%>)

    ; the world in which the controllers will live
    (init-field w)   ; World<%>

    ; the model to which the controllers will be connected
    (init-field m)   ; Model<%>

    (super-new)

    ; KeyEvent -> Void
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "z") (add-viewer Controller-XY%)]
        ;[(key=? kev "p") (add-viewer PositionController%)]
        ))


    (define/public (add-viewer viewer-class)
      (send w add-stateful-widget (new viewer-class [model m])))

    (define/public (add-to-scene s) s)
    
    (define/public (after-tick) 'controller-factory-after-tick-trap)

    (define/public (after-button-down mx my)
      this)
    (define/public (after-drag mx my)
      this)
      (define/public (after-move mx my)
      this)
    (define/public (after-button-up mx my)
      this)

    ))