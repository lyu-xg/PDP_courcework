#lang racket
(require rackunit)
(require "extras.rkt")
(require "interface.rkt")
(require "face-images.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(provide Politician%)

;===============================================================================
;; CONSTANTS

(define REPEL-DISTANCE 75)
(define SPEED 6)
(define REPEL-SPEED -100)

(define POLITICIAN-FACE-1 face1)
(define POLITICIAN-FACE-2 face2) 
;; Interp: the images representing the faces of politician

;===============================================================================
;; POLITICIAN CLASS


;; Constructor template:
;; (new Clock% [x Integer][y Integer] [selected? Boolean]
;;             [prev-mx Integer][prev-my Integer]
;;             [tick Integer] )  ;; all init fields are optional
;; A Politician represents a politician on the canvas.

(define Politician%
  (class* object% (Toy<%>)
    ;===========================================================================
    (init-field [face1? true])
    ;; Boolean, represents which face image to display, default display face1
    ;===========================================================================
    (init-field [x INIT-X]   ;; Number, x pixels of center from left
                [y INIT-Y])  ;; Number, y pixels of center from top
    ;===========================================================================
    (init-field [vx 0]   ;; Number ∈ [-1, 1] 
                [vy -1]) ;; Number ∈ [-1, 1]
    ;; (vx, vy) is a directional vector which initially pointing north
    ;; WHERE: (mathematically) vx^2 + vy^2 = 1
    ;===========================================================================
    (init-field [prev-mx -1000] [prev-my -1000]) ;; Integer Integer
    ;; set the mouse position to negative when no mouse event detected
    ;; in order not to repel the politician when no mouse is present
    ;===========================================================================
    (super-new)
    ;===========================================================================
    ;; toy-x: -> Int
    ;; GIVEN : No inputs required.
    ;; EXAMPLE: (send (new Politician%) toy-x) -> INIT-X
    ;; RETURNS : the x-component of the center of this politician.
    (define/public (toy-x) x)
    ;===========================================================================
    ;;toy-y: -> Int
    ;;GIVEN : No inputs required.
    ;; EXAMPLE: (send (new Politician%) toy-y) -> INIT-Y
    ;;RETURNS : the y-component of the center of this politician.
    (define/public (toy-y) y)
    ;===========================================================================
    ;; toy-data: -> Int
    ;; GIVEN : No inputs are required
    ;; EXAMPLE:
    ;;   (send (new Politician% [prev-mx INIT-X][prev-my INIT-Y]) toy-data) -> 0
    ;; RETURNS : The distance between mouse position and this Politician.
    ;; Strategy: combine simler functions
    (define/public (toy-data) (floor (euclidean-distance x y prev-mx prev-my)))
    ;===========================================================================
    ;; add-to-scene: Scene -> Scene
    ;; RETURNS: a scene like the given one,
    ;;          but with this politician painted on it.
    ;; EXAMPLE: see test cases for example
    ;; Strategy: case on face1?
    (define/public (add-to-scene scene)
      (if face1?
          (place-image POLITICIAN-FACE-1 x y scene)
          (place-image POLITICIAN-FACE-2 x y scene)))
    ;===========================================================================
    ;; after-move:         Integer Integer -> Toy
    ;; after-button-down:  Integer Integer -> Toy
    ;; after-button-up:    Integer Integer -> Toy
    ;; after-drag:         Integer Integer -> Toy
    ;; GIVEN: the location of a drag event
    ;; RETURNS: the same Politician after mouse event
    ;; EXAMPLE: see test cases for example
    ;; STRATEGY: use constructor template for Politician
    (define/public (after-move mx my)
      (new Politician%
           [x x] [y y] [face1? face1?]
           [prev-mx mx] [prev-my my]
           [vx (update-vx mx my)]
           [vy (update-vy mx my)]))
    (define/public (after-button-down mx my) (send this after-move mx my))
    (define/public (after-button-up mx my)   (send this after-move mx my))
    (define/public (after-drag mx my)        (send this after-move mx my))
    ;===========================================================================
    ;; update-vx: Integer Integer -> Number ∈ [-1, 1] 
    ;; update-vy: Integer Integer -> Number ∈ [-1, 1] 
    ;; GIVEN: the location of a mouse event
    ;; RETURNS: the updated vx/vy value
    ;; EXAMPLE:
    ;;   (send (new Politician% [prev-mx INIT-X][prev-my 0]) update-vx)
    ;;      -> 0
    ;;   (send (new Politician% [prev-mx INIT-X][prev-my 0]) update-vx)
    ;;      -> -1
    ;; Strategy: combine simpler functions
    (define (update-vx mx my)
      (if (= mx x) 0
          (let [(abs-value (vx-value mx my))]
            (if (> mx x) abs-value
                (- 0 abs-value)))))
    (define (update-vy mx my)
      (if (= my y) 0
          (let [(abs-value (vy-value mx my))]
            (if (> my y) abs-value
                (- 0 abs-value)))))
    ;===========================================================================
    ;; vx-value: Integer Integer -> Number ∈ [0, 1] 
    ;; vy-value: Integer Integer -> Number ∈ [0, 1] 
    ;; GIVEN: the location of a mouse event
    ;; RETURNS: the absolute updated vx/vy value
    ;; EXAMPLE:
    ;;   (send (new Politician% [prev-mx INIT-X][prev-my 0]) vx-value)
    ;;      -> 0
    ;;   (send (new Politician% [prev-mx INIT-X][prev-my 0]) vy-value)
    ;;      -> 1
    ;; Strategy: combine simpler functions
    (define (vx-value mx my)
      (if (= my y) 1
          (let [(k (sqr (/ (- mx x) (- my y))))]
            (sqrt (/ k (+ 1 k))))))
    (define (vy-value mx my)
      (if (= mx x) 1
          (let [(k (sqr (/ (- my y) (- mx x))))]
            (sqrt (/ k (+ 1 k))))))
    ;===========================================================================
    ;; after-key-event : KeyEvent -> Toy
    ;; RETURNS: this politician without any change
    ;; EXAMPLE: see test cases for example
    ;; NOTE: politician does not respond to any keyevent
    ;; Strategy: return this object
    (define/public (after-key-event kev) this)
    ;===========================================================================
    ;; after-tick : -> Toy
    ;; RETURNS: this politician with location&face updated by one tick
    ;; EXAMPLE: see test cases for example
    ;; Strategy: use constructor template for Politician
    (define/public (after-tick)
      (let [(repel? (repel-politician? prev-mx prev-my))]
        (new Politician%
             [x (+ x (* vx (if repel? REPEL-SPEED SPEED)))]
             [y (+ y (* vy (if repel? REPEL-SPEED SPEED)))]
             [vx vx] [vy vy]
             [prev-mx prev-mx] [prev-my prev-my]
             [face1? (if repel? (not face1?) face1?)])))
    ;===========================================================================
    ;; repel-politician?: Integer Integer -> Boolean
    ;; RETURNS: whether should the politician be repelled under given mx,my
    ;; EXAMPLE: see test cases for example
    ;; Strategy: combine simpler functions
    (define/public (repel-politician? mx my)
      (< (euclidean-distance mx my x y) REPEL-DISTANCE))
    ;===========================================================================
    ;; for-test methods
    ;; Strategy: return values stored in the field of this object
    ;; EXAMPLE: (send (new Politician%) for-test:get-vx) -> 0
    ;;          (send (new Politician%) for-test:get-vy) -> -1
    (define/public (for-test:get-vx) vx)
    (define/public (for-test:get-vy) vy)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(begin-for-test
  (local
    ((define p0 (new Politician%))
     (define p1 (send (send p0 after-button-up INIT-X INIT-Y)
                      after-drag INIT-X INIT-Y))
     (define p2 (send p0 after-button-up INIT-X INIT-Y)))
    (check-equal?
     (send p1 toy-x) INIT-X
     "if mouse position is the center position, x should remain still")
    (check-equal?
     (send p1 toy-y) INIT-Y
     "if mouse position is the center position, y should remain still")
    (check-equal?
     (send p1 toy-data) 0
     "if mouse position is the center position, toy data should be 0")
    (check-equal?
     (send p0 add-to-scene EMPTY-CANVAS)
     (send p1 add-to-scene EMPTY-CANVAS)
     "p0 and p1 should be same in appearance")
    (check-equal?
     (send (send p1 after-tick) add-to-scene EMPTY-CANVAS)
     (send (send p2 after-tick) add-to-scene EMPTY-CANVAS)
     "p1-afte-tick and p1-after-tick should be same in appearance"))
  (local
    ((define p0 (new Politician% [x 10][y 10]))
     (define p1 (send (send p0 after-key-event "s") after-button-down 20 20))
     (define p2 (send p0 after-button-down 5 5))
     (define p3 (send p0 after-button-down 5 10))
     (define p4 (send p0 after-button-down 10 500))
     (define p5 (send p4 after-tick)))
    (check-equal?
     (send p1 for-test:get-vx)
     (send p1 for-test:get-vy)
     "if direction vector's slope is exactly 1, vx should equal to vy")
    (check-equal?
     (send p2 for-test:get-vx)
     (send p2 for-test:get-vy)
     "if direction vector's slope is exactly 1, vx should equal to vy")
    (check-equal?
     (send p5 toy-x) 10
     "p5-after-tick should not change on x")))
