#lang racket
(require "GeneralController.rkt")
(require "extras.rkt")
(require "interfaces.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(provide VisualizedController%)
;===============================================================================
(define VisualizedController%
  (class* GeneralController% (Controller<%>)
    ;===========================================================================
    (inherit-field model particle controller-selected? saved-mx saved-my)
    ;; INTERP: see GeneralController.rkt
    ;===========================================================================
    (super-new)
    ;===========================================================================
    ;; All VisualizedController does not responde to any key event
    (define/override (after-key-event kev) 'nothing)
    ;===========================================================================
    ;; update-ticking : -> Void
    ;; NOTE: this method is only called when button-down
    ;; EFFECT: stops the model from ticking
    ;; Strategy: combine simpler functions
    (define/override (update-ticking)
      (send model update (make-tick-indicator false)))
    ;===========================================================================
    ))
;===============================================================================
;; TESTS: see tests.rkt
