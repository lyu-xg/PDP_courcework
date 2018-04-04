#lang racket

(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)
(provide CANVAS-WIDTH
         CANVAS-HEIGHT
         EMPTY-CANVAS
         INIT-X
         INIT-Y
         NEW-THROBBER-EVENT
         NEW-CLOCK-EVENT
         NEW-POLITICIAN-EVENT)



(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
;; Interp: the size of the canvas is always 500*600

(define INIT-X (/ CANVAS-WIDTH 2))
(define INIT-Y (/ CANVAS-HEIGHT 2))
(define NEW-THROBBER-EVENT "t")
(define NEW-CLOCK-EVENT "c")
(define NEW-POLITICIAN-EVENT "p")