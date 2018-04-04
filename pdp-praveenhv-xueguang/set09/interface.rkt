#lang racket
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)
(provide Metatoy<%> Toy<%>
         EMPTY-CANVAS
         CANVAS-WIDTH CANVAS-HEIGHT
         INIT-X INIT-Y
         euclidean-distance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONTANTS

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
;; Interp: the size of the canvas is always 500*600

(define INIT-X (/ CANVAS-WIDTH 2))
(define INIT-Y (/ CANVAS-HEIGHT 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS

;; euclidean-distance : Int Int Int Int -> Number
;; GIVEN: two Postions represented by 4 Integers
;; RETURNS: the euclidean distance of the two Positions
;; Strategy: combine simpler functions
(define (euclidean-distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACES

;===============================================================================
;; parent interface of Matatoy<%>

(define World<%>
  (interface ()

    ; -> World
    ; GIVEN: no arguments
    ; RETURNS: the state of the world at the next tick
    after-tick          

    ; Integer Integer MouseEvent-> World
    ; GIVEN: a location
    ; RETURNS: the state of the world that should follow the
    ; given mouse event at the given location.
    after-mouse-event

    ; KeyEvent : KeyEvent -> Widget
    ; GIVEN: a key event
    ; RETURNS: the state of the world that should follow the
    ; given key event
    after-key-event     

    ; -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene
    ))

;===============================================================================
;; A Metatoy is one and only object of any class that implements Metatoy<%>.
;; big-bang will communicate with the world through the Metatoy<%> interface.

(define Metatoy<%>
  (interface 
  
   ;; the (World<%>) says that Metatoy<%> inherits from World<%>
   ;; This means that any class that implements Metatoy<%> must
   ;; implement all the methods from World<%> plus all the methods
   ;; defined here. In this case, there is just one additional method,
   ;; called get-toys.
   (World<%>)

    ;; -> ListOfToy
    get-toys
    ))

;===============================================================================
;; parent interface for Toy<%>

(define Widget<%>
  (interface ()

    ; -> Widget
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          

    ; Integer Integer -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag

    ; KeyEvent : KeyEvent -> Widget
    ; GIVEN: a key event and a time
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))

;===============================================================================
;; A Toy is an object of any class that implements Toy<%>
;; I have three such classes, one for each kind of toy. 

(define Toy<%> 
  (interface
  
   ;; The interface Toy<%> inherits from the interface Widget<%>.
   ;; This means that any class that implements Toy<%> must implement
   ;; all the methods from Widget<%> plus all the methods defined here.
   (Widget<%>)


    ;; Note: the Widgets of the space-invader-examples don't respond
    ;; to mouse "move" events, but some of our toys do.  So we add an
    ;; after-move method to the interface.

    ;;  Int Int -> Toy
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    after-move

 
    ;; -> Int
    ;; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y

    ;; -> Int
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a politician, it is the current distance to the mouse
    toy-data
    ))