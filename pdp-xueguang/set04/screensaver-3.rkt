;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         world-paused?
         new-circle
         circ-x
         circ-y
         circ-vx
         circ-vy
         world-after-mouse-event
         circ-after-mouse-event
         circ-selected?
         world-circles
         circle-after-key-event
         circle-pen-down?
         )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct position (x y))
;; A Position is a (make-postion (NonNegInteger NonNegInteger))
;; template:
;; (define (postion-fn pos)
;;   ...
;;   (position-x pos)
;;   (position-y pos))

(define-struct velocity (vx vy))
;; A Velocity is a (make-velocity (Integer Integer))
;; template:
;; (define (velocity-fn v)
;;   ...
;;   (velocity-vx v)
;;   (velocity-vy v))

(define-struct distance (dx dy))
;; A Distance is a (make-distance (Integer Integer))
;; Interp: Distance is used in a Circle to represent
;;         how far is the Circle from mouse-down location
;; template:
;; (define (distance-fn d)
;;   ...
;;   (distance-dx d)
;;   (distance-dy d))

;; A ListOfPosition can be one of
;; -- empty
;; -- (cons Position ListOfPosition)
;; template:
;; (define (lop-fn lop)
;;   (if (empty? lop) (...)
;;       (... (first lop) (rest lop)...)))

;; A ListOfCircle can be one of
;; -- empty
;; -- (cons Circle ListOfCircle)
;; template:
;; (define (loc-fn loc)
;;   (if (empty? loc) (...)
;;       (... (first loc) (rest loc)...)))

(define-struct circ (pos v d selected? pen? inks))
;; A Circle is a
;;   (make-circ Position Velocity Distance Boolean Boolean ListOfPosition)
;; Interp:
;;  -- Position is the circle position on the canvas
;;  -- Velocity is the circle velocity used for calculate next tick position
;;  -- Distance is the offset between the center and the mouse drag event position
;;  -- Boolean indicate whether the circle is selcted by mouse event
;;  -- Boolean indicate whether the circle has its pen down
;;     if pen down, circle position will be added into inks after unselected tick
;;  -- ListOfPosition is the pen marks, each represented by a Position
;; template:
;; (define (circ-fn c)
;;   ...
;;   (postion-fn (circ-pos c))
;;   (velocity-fn (circ-v c))
;;   (distance-fn (circ-d))
;;   (circ-selected? c)
;;   (circ-pen? c)
;;   (circ-lop c))


(define-struct world (loc paused? pos))
;; A WorldState is a (make-world ListOfCircle Boolean Position)
;; NOTE: the phrase "WorldState" and "World" are interchangable
;; Interp:
;;   -- ListOfCircle represents the circles in the WorldState (initially empty)
;;   -- Boolean indicates whether the world is paused (initially true)
;;   -- Position represents the mouse event position
;; template:
;; (define (world-fn w)
;;   ...
;;   (loc-fn (world-loc w))
;;   (world-paused? w))
;;   (position-fn (world-pos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define RADIUS 40)
;; Interp: the radius of the two Circles will always be 40

(define NORMAL-CIRCLE (circle RADIUS "outline" "blue"))
;; Interp: A normal Circle should be outlined blue
(define SELECTED-CIRCLE (circle RADIUS "outline" "red"))
;; Interp: A selected Circle should be outlined red

(define SELECTION-DOT (circle 5 "solid" "red"))
;; Interp: the selection dot image on mouse press 

(define INK-MARK (circle 1 "solid" "black"))
;; Interp: a ink mark which will be left by a pen down circle after a tick


(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
;; Interp: the size of the canvas is always 400*300 


(define LEFT-BOUND RADIUS)
(define UPPER-BOUND RADIUS)
(define LOWER-BOUND (- CANVAS-HEIGHT RADIUS))
(define RIGHT-BOUND (- CANVAS-WIDTH RADIUS))
;; Interp: the domain of legal x and y values for Position

(define INIT-CIRCLE (make-circ (make-position
                                (/ CANVAS-WIDTH 2)
                                (/ CANVAS-HEIGHT 2))
                               (make-velocity 0 0)
                               (make-distance 0 0)
                               false false empty))

(define AC-FACTOR 2)
;; Interp: acceleration factor is 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN

;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the world
(define (screensaver speed)
  (big-bang (initial-world 1)
            (on-tick world-after-tick speed)
            (on-key world-after-key-event)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)))


;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; Strategy: Use data definition of WorldState
(define (initial-world a) (make-world empty true (make-position -100 -100)))
;; Interp: when no mouse event, position set to negative to hide the red dot


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WORLD-AFTER-TICK STUFF

;; world-after-tick : WorldState -> WorldState
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; Strategy: case on world-paused?
(define (world-after-tick w)
  (if (not (world-paused? w))
  (make-world (circles-after-tick (world-loc w))
              (world-paused? w) (world-pos w)) w))


;; circles-after-tick : ListOfCircle -> ListOfCircle
;; RETURNS: a ListOfCircle that each Circle is updated by a tick
(define (circles-after-tick loc)
  (if (empty? loc) empty
      (cons (circle-after-tick (first loc))
            (circles-after-tick (rest loc)))))



;; circle-after-tick : Circle -> Circle
;; GIVEN: a Circle
;; RETURNS: the Circle state that should follow the given Circle after a tick
;; Strategy: case on whether the circle is selected or not
(define (circle-after-tick c)
  (if (circ-selected? c) c
      (make-circ (make-position (x-after-tick c) (y-after-tick c))
                 (make-velocity (vx-after-tick c) (vy-after-tick c))
                 (circ-d c)
                 false (circ-pen? c) (inks-after-tick c))
      ))
;; example:
;; (circle-after-tick (new-circle 45 80 -15 20)) => (new-circle 40 100 15 20)


;; x-after-tick : Circle -> NonNegInteger
;; GIVEN: a Circle
;; RETURNS: the next x value of the Circle according to it's velocity
;; Strategy: case on whether x value goes beyond boundaries 
(define (x-after-tick c)
  (if (<= (+ (circ-x c) (circ-vx c)) LEFT-BOUND)
      LEFT-BOUND
      (min RIGHT-BOUND (+ (circ-x c) (circ-vx c)))))

;; y-after-tick : Circle -> NonNegInteger
;; GIVEN: a Circle
;; RETURNS: the next y value of the Circle according to it's velocity
;; Strategy: case on whether y value goes beyond boundaries
(define (y-after-tick c)
  (if (<= (+ (circ-y c) (circ-vy c)) UPPER-BOUND)
      UPPER-BOUND
      (min LOWER-BOUND (+ (circ-y c) (circ-vy c)))))

;; vx-after-tick : Circle -> Integer 
;; GIVEN: a Circle
;; RETURNS: the next vx value of the Circle according to it's position
;; Strategy: case on whether x value goes beyond boundaries
(define (vx-after-tick c)
  (if (or (<= (+ (circ-x c) (circ-vx c)) LEFT-BOUND)
           (>= (+ (circ-x c) (circ-vx c)) RIGHT-BOUND))
      (- 0 (circ-vx c)) 
      (circ-vx c)))

;; vy-after-tick : Circle -> Integer 
;; GIVEN: a Circle
;; RETURNS: the next vy value of the Circle according to it's position
;; Strategy: case on whether y value goes beyond boundaries
(define (vy-after-tick c)
  (if (or (<= (+ (circ-y c) (circ-vy c)) UPPER-BOUND)
           (>= (+ (circ-y c) (circ-vy c)) LOWER-BOUND))
      (- 0 (circ-vy c))
      (circ-vy c)))

;; inks-after-tick : Circle -> ListOfPosition
;; GIVEN: A Circle
;; RETURNS: a ListOfPosition with the inks state updated after a tick
;; Strategy: case on whether the cirlce is not selected and pen down
(define (inks-after-tick c)
  (if (and (circ-pen? c) (not (circ-selected? c)))
      (cons (circ-pos c) (circ-inks c))
      (circ-inks c)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WORLD-AFTER-KEY-EVENT STUFF

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; RETURNS: the WorldState that should follow the given worldstate
;;          after the given keyevent
;; Strategy: combine simpler functions
(define (world-after-key-event w key)
  (make-world (circles-after-key-event (world-loc w) key)
              (paused-after-key-event w key)
              (world-pos w)))

;; paused-after-key-event : WorldState KeyEvent -> Boolean
;; RETURNS: the paused? state of the WorldState after the given KeyEvent
;; Strategy: case on whether the KeyEvent is the space bar
(define (paused-after-key-event w key)
  (if (key=? key " ") (not (world-paused? w)) (world-paused? w)))


;; circles-after-key-event : ListOfCircle KeyEvent-> ListOfCirlce
;; RETURNS: a List of Circle where each circle state is updated
;;          according to the given key event
;; Strategy: case on whether the event is a NewCircle event
(define (circles-after-key-event loc key)
  (if (key=? key "n") (cons INIT-CIRCLE loc)
      (each-circle-after-key-event loc key)))


;; circles-after-key-event : ListOfCircle KeyEvent-> ListOfCirlce
;; RETURNS: a List of Circle where each circle state is updated
;;          according to the given key event
;; Strategy: case on the key event
(define (each-circle-after-key-event loc key)
  (if (empty? loc) empty
      (cons (circle-after-key-event (first loc) key)
            (each-circle-after-key-event (rest loc) key))))


;; circle-after-key-event : Circle KeyEvent -> Circle
;; RETURNS: the state of the circle that should follow the given
;; circle after the given key event

(define (circle-after-key-event c key)
  (cond
    [(key=? key "d")     (circle-after-d c)]
    [(key=? key "u")     (circle-after-u c)]
    [(key=? key "e")     (circle-after-e c)]
    [(key=? key "left")  (circle-after-left c)]
    [(key=? key "right") (circle-after-right c)]
    [(key=? key "up")    (circle-after-up c)]
    [(key=? key "down")  (circle-after-down c)]
    [else c]))



;; circle-after-d : Circle -> Circle
;; RETURNS: the state of the given Circle after PenDown event
;; Strategy: case on whether the circle is selected
(define (circle-after-d c)
  (if (circ-selected? c)
      (make-circ (circ-pos c) (circ-v c) (circ-d c)
                 true true (circ-inks c))
      c))


;; circle-after-u : Circle -> Circle
;; RETURNS: the state of the given Circle after a PenUp event
;; Strategy: case on whether the Circle is selected and also pen-down
(define (circle-after-u c)
  (if (and (circ-pen? c) (circ-selected? c))
      (make-circ (circ-pos c) (circ-v c) (circ-d c)
                 true false (circ-inks c))
      c))

;; circles-after-e : Circle -> Circle
;; RETURNS: the state of the given Circle after a EraseMark event
;; Strategy: case on whether the Circle is selected
(define (circle-after-e c)
  (if (circ-selected? c)
      (make-circ (circ-pos c) (circ-v c) (circ-d c)
                 true (circ-pen? c) empty)
      c))

;; circles-after-left : Circle -> Circle
;; RETURNS: the state of the given Circle after a AccelerateLeft event
;; Strategy: case on whether the Circle is selected
(define (circle-after-left c)
  (if (circ-selected? c)
      (make-circ (circ-pos c)
                 (make-velocity (- (circ-vx c)  AC-FACTOR)
                                (circ-vy c))
                 (circ-d c) true (circ-pen? c) (circ-inks c))
      c))

;; circles-after-right : Circle -> Circle
;; RETURNS: the state of the given Circle after a AccelerateRight event
;; Strategy: case on whether the Circle is selected
(define (circle-after-right c)
  (if (circ-selected? c)
      (make-circ (circ-pos c)
                 (make-velocity (+ (circ-vx c) AC-FACTOR)
                                (circ-vy c))
                 (circ-d c) true (circ-pen? c) (circ-inks c))
      c))

;; circles-after-up : Circle -> Circle
;; RETURNS: the state of the given Circle after a AccelerateUp event
;; Strategy: case on whether the Circle is selected
(define (circle-after-up c)
  (if (circ-selected? c)
      (make-circ (circ-pos c)
                 (make-velocity (circ-vx c)
                                (- (circ-vy c)  AC-FACTOR))
                 (circ-d c) true (circ-pen? c) (circ-inks c))
      c))

;; circles-after-down : Circle -> Circle
;; RETURNS: the state of the given Circle after a AccelerateDown event
;; Strategy: case on whether the Circle is selected
(define (circle-after-down c)
  (if (circ-selected? c)
      (make-circ (circ-pos c)
                 (make-velocity (circ-vx c)
                                (+ (circ-vy c) AC-FACTOR))
                 (circ-d c) true (circ-pen? c) (circ-inks c))
      c))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WORLD-TO-SCENE STUFF

;; world-to-scene : WorldState -> Scene
;; RETURNS: a Scene that protrays the given world
;; Strategy: Use tempalte for WorldState
(define (world-to-scene w)
  (place-image  SELECTION-DOT
                (position-x (world-pos w))
                (position-y (world-pos w))
                (draw-circles (world-loc w))))


;; draw-circle : Circle  -> Image
;; GIVEN: a Circle
;; RETURNS: a image of the circle with the velocities displayed
;; Strategy: case on whether the Circle is selected
(define (get-circle-image c)
  (overlay (if (circ-selected? c) SELECTED-CIRCLE NORMAL-CIRCLE)
           (format-velocity (circ-v c))))

;; draw-inks : ListOfPosition Scene -> Scene
;; GIVEN: a ListOfPosition representing the ink marks
;;        and a Scene to put the inks on
;; RETURNS: a Scene of the inks, each drawed on the canvas
;; Strategy: case on whether the List is empty
(define (draw-inks inks canv)
  (if (empty? inks) canv
      (place-image INK-MARK
                   (position-x (first inks))
                   (position-y (first inks))
                   (draw-inks (rest inks) canv))))


;; draw-circles : ListOfCircle -> Scene
;; GIVEN: a ListOfCircle
;; RETURNS: a Scene with the all the Circles and their ink marks
;; Strategy: case on whether the List is empty
(define (draw-circles loc)
  (if (empty? loc) EMPTY-CANVAS
      (place-image (get-circle-image (first loc))
                   (circ-x (first loc))
                   (circ-y (first loc))
                   (draw-inks (circ-inks (first loc))
                                (draw-circles (rest loc))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WORLD-AFTER-MOUSE-EVENT STUFF

;; world-after-mouse-event
;;  : WorldState Int Int MouseEvent -> WorldState
;; GIVEN: A World, the x- and y-coordinates of a mouse event, and the
;;        mouse event
;; RETURNS: the world that should follow the given world after the given mouse
;;          event.
;; Strategy: case on the mouse event + combine simpler functions
(define (world-after-mouse-event w mx my mev)
  (make-world (circles-after-mouse-event (world-circles w) mx my mev)
              (world-paused? w)
              (pos-after-mouse-event (world-pos w) mx my mev)))


;; pos-after-mouse-event : Position Int Int MouseEvent -> Position
;; GIVEN: the Position representing the red dot,
;;        the x- and y-coordinates of a mouse event, and the mouse event
;; RETURNS: the Position updated according to the mouse event given
;; Strategy: case on whether the event is button-up or not
(define (pos-after-mouse-event pos x y mev)
  (cond 
        [(or (mouse=? mev "button-down") (mouse=? mev "drag"))
         (make-position x y)]
        [else
         (make-position -100 -100)]))

;; circles-after-mouse-event : ListOfCircle Int Int MouseEvent -> ListOfCircle
;; GIVEN: a List of Circle and the x- and y-coordinates of a mouse event,
;;        and the mouse event
;; RETURNS: the List of Circles each updated according to the mouse event
;; Strategy: case on whether the List is empty
(define (circles-after-mouse-event loc x y mev)
  (if (empty? loc) empty
      (cons (circ-after-mouse-event (first loc) x y mev)
            (circles-after-mouse-event (rest loc) x y mev))))


;; circ-after-mouse-event : Circle Int Int MouseEvent -> Circle
;; GIVEN: a Circle and the x- and y-coordinates of a mouse event,
;;        and the mouse event
;; RETURNS: the Circle updated according to the given mouse event
;; Strategy: case on the mouse event
(define (circ-after-mouse-event c x y mev)
  (cond
    [(mouse=? mev "button-down") (circle-after-button-down c x y)]
    [(mouse=? mev "drag") (circle-after-drag c x y)]
    [(mouse=? mev "button-up")(circle-after-button-up c)][else c]))


;; circle-after-button-down : Circle Int Int MouseEvent -> Circle
;; GIVEN: a Circle and the location of the button-down
;; REUTRNS: the Circle following a button-down at the given location
;;          if the button-down location is inside the Circle
;;          then return the selected Circle
;; Strategy: case on whether the mouse location is inside circle
(define (circle-after-button-down c x y)
  (if (in-circle? c x y)
      (make-circ (circ-pos c)(circ-v c)
                 (make-distance (- x (circ-x c))
                                (- y (circ-y c)))
                 true (circ-pen? c) (circ-inks c))
      c))

;; circle-after-drag : Circle Int Int MouseEvent -> Circle
;; GIVEN: a Circle and the location of the drag event
;; RETURNS: the Circle after a mouse drag event
;; Strategy: case on whether the mouse location is inside the Circle
(define (circle-after-drag c x y)
  (if (circ-selected? c)
      (make-circ (make-position
                  (- x (distance-dx (circ-d c)))
                  (- y (distance-dy (circ-d c))))
                 (circ-v c) (circ-d c)
                 true (circ-pen? c) (circ-inks c))
      c))


;; unselect-circle : Circle -> Circle
;; GIVEN: a Circle
;; RETURNS: the same Circle but not selected
(define (unselect-circle c)
  (make-circ (circ-pos c) (circ-v c) (circ-d c) false (circ-pen? c) (circ-inks c)))
;; (unselect-circle INIT-CIRCLE1) => INIT-CIRCLE1


;; circle-after-button-up : Circle Int Int MouseEvent -> Circle
;; GIVEN: a Circle and the location of the button-up event
;; RETURNS: the same Circle except unselected
(define circle-after-button-up unselect-circle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPER FUNCTIONS

;; format-velocity : Velocity -> Image
;; GIVEN: a Velocity
;; RETURNS a Image containing the Velocity
;; Strategy: use template for Velocity + combine simpler functions
(define (format-velocity v)
  (text (string-append
         "("
         (number->string (velocity-vx v))
         ","
         (number->string (velocity-vy v))
         ")") 14 "blue"))



;; select-circle : Circle -> Circle
;; GIVEN: a Circle
;; RETURNS: the same Circle but selected
(define (select-circle c)
  (make-circ (circ-pos c) (circ-v c) (circ-d c) true (circ-pen? c) (circ-inks c)))
;; (select-circle INIT-CIRCLE1)
;; => (make-circ (make-position 200 100) (make-velocity -12 20) (make-distance 0 0) true)



;; euclidean-distance : Position Position -> number
;; GIVEN: two Postions
;; RETURNS: the euclidean distance of the two Positions
;; Strategy: use template for Position + combine simpler functions
(define (euclidean-distance p1 p2)
  (sqrt (+ (sqr (- (position-x p1) (position-x p2)))
           (sqr (- (position-y p1) (position-y p2))))))
;; examples:
;; (euclidean-distance (make-position 0 0) (make-position 3 4)) => 5

;; in-circle: Circie Integer Integer -> Boolean
;; GIVEN: a Circle and mouse positions as Integers
;; RETURNS: true iff the mouse positions is inside the circle
;; Strategy: use template for Circle + combine simpler functions
(define (in-circle? c mx my)
  (>= RADIUS (euclidean-distance (circ-pos c) (make-position mx my))))
;; examples:
;; (in-circle? INIT-CIRCLE1 201 101) => true
;; (in-circle? INIT-CIRCLE1 241 100) => false

;; circ-x : Circle -> NonNegInt
;; circ-y : Circle -> NonNegInt
;; circ-vx : Circle -> Int
;; circ-vy : Circle -> Int
;; RETURNS: the coordinates of the center of the circle and its
;;          velocity in the x- and y- directions.
;; Strategy: use template for Position, Velocity and Circle
(define (circ-x c) (position-x (circ-pos c)))
;; example: (circ-x INIT-CIRCLE1) => 200
(define (circ-y c) (position-y (circ-pos c)))
;; example: (circ-y INIT-CIRCLE1) => 100
(define (circ-vx c) (velocity-vx (circ-v c)))
;; example: (circ-vx INIT-CIRCLE1) => -12
(define (circ-vy c) (velocity-vy (circ-v c)))
;; example: (circ-vy INIT-CIRCLE1) => 20


;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: an UNSELECTED circle centered at the center of canvas
(define (new-circle x y vx vy)
  (make-circ (make-position x y)
             (make-velocity vx vy)
             (make-distance 0 0)
             false false empty))
;; example:
;; (new-circle 100 100 -12 20 false) => INIT-CIRCLE

;; world-circles : WorldState -> ListOfCircle
;; RETURNS: the specified attribute of the WorldState
;; Strategy: use template for WorldState
(define (world-circles w) (world-loc w))

;; circle-pen-down? : Circle -> Boolean
;; RETURNS: true if the pen in the given circle is down
;; Strategy: use template for Circle
(define (circle-pen-down? c) (circ-pen? c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTING HELPERS
(define SPEEDY-CIRCLE (unselect-circle
                       (circle-after-d
                        (select-circle (new-circle 200 150 10 10)))))
(define SELECTED-INIT (select-circle INIT-CIRCLE))
(define MOVING-PENDOWN (unselect-circle (circle-after-d SELECTED-INIT)))
(define TWO-CIRCLES (list SELECTED-INIT INIT-CIRCLE))

(define MOVING-CIRCLE-WORLD (make-world (cons MOVING-PENDOWN empty)
                                        false (make-position -100 -100)))

(define SPEEDY-CIRCLE-WORLD (make-world (cons SPEEDY-CIRCLE empty)
                                        false (make-position -100 -100)))

(define SPEEDY-WORLD-BUTTON-DOWN
  (world-after-mouse-event SPEEDY-CIRCLE-WORLD
                           (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) "button-down"))

(define SPEEDY-WORLD-BUTTON-DOWN-WRONG-PLACE
  (world-after-mouse-event SPEEDY-CIRCLE-WORLD
                           CANVAS-WIDTH CANVAS-HEIGHT "button-down"))

(define DRAG-SPEEDY-WORLD
  (world-after-mouse-event SPEEDY-WORLD-BUTTON-DOWN 200 300 "drag"))

(define DRAG-UNSELECTED
  (world-after-mouse-event SPEEDY-WORLD-BUTTON-DOWN-WRONG-PLACE
                           200 300 "drag"))

(define BUTTON-UP-AFTER-DRAG
  (world-after-mouse-event DRAG-SPEEDY-WORLD 0 0 "button-up"))


(define PAUSED-MOVING-WORLD (make-world (cons SPEEDY-CIRCLE empty)
                                        true (make-position -100 -100)))

(define (apply-key-events loc lok)
  (if (empty? lok) loc
      (apply-key-events (circles-after-key-event loc (first lok)) (rest lok))))

;(define (world-events w lok)
;  (if (empty? lok) w
;      (make-world (apply-key-events (world-circles w) lok)
;                  false (make-position -100 -100))))

(define CIRCLE-WITH-INKS  (select-circle (circle-after-tick
                              (circle-after-tick
                               (circle-after-tick SPEEDY-CIRCLE)))))


;; TESTS

;(screensaver .5)
(begin-for-test
  (check-equal? (in-circle? INIT-CIRCLE 201 151)
                true
                "in-circle? should return true if given coordinate is within the circle")
  (check-equal? (in-circle? INIT-CIRCLE 241 150)
                false
                "in-circle? should return false if given coordinate is outside the circle")
  (check-equal? (new-circle (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) 0 0)
                INIT-CIRCLE
                "new circle should be in the center with no velocity")
  (check-equal? (circle-after-tick INIT-CIRCLE) INIT-CIRCLE
                "initial circle should not move initially")
  (check-equal? (length (circ-inks INIT-CIRCLE)) 0
                "initial circle should have no ink marks")
  (check-equal? (length (circ-inks (circle-after-tick MOVING-PENDOWN)))
                1 "after a pen down tick, the length of inks should be 1")
  (check-equal? (circle-pen-down? MOVING-PENDOWN) true
                "circle-pen-down? should return true for pen-down circle")
  (check-equal? (length (circ-inks
                         (first (world-circles (world-after-tick MOVING-CIRCLE-WORLD)))))
                1 "after a pen down tick, the length of first cirlce inks should be 1")
  (check-equal? (apply-key-events TWO-CIRCLES (list "up" "down" "left" "right"))
                TWO-CIRCLES
                "after accelerating all directions, velocity should remain same")
  (check-equal? (apply-key-events TWO-CIRCLES (list "d" "u" "e" "n"))
                (cons INIT-CIRCLE TWO-CIRCLES)
                "after these events, the list should only change by adding a new circ")
  (check-equal? (world-to-scene MOVING-CIRCLE-WORLD)
                (world-to-scene (world-after-key-event (initial-world 1) "n"))
                "INITIAL WORLD with one circle should be not changed after drawing")
  (check-equal? (draw-circles (list CIRCLE-WITH-INKS))
                (draw-circles (cons (select-circle (circle-after-tick (circle-after-tick
                                                  (circle-after-tick SPEEDY-CIRCLE)))) empty))
                "both scene should have same dots")
  (check-equal? (world-paused? (world-after-key-event PAUSED-MOVING-WORLD " "))
                false
                "space key event should unpause the given paused world")
  (check-equal? (circ-selected? (first (world-circles SPEEDY-WORLD-BUTTON-DOWN)))
                true
                "mouse button down on the circle should select the circle")
  (check-equal? (circ-selected? (first (world-circles SPEEDY-WORLD-BUTTON-DOWN-WRONG-PLACE)))
                false
                "mouse button down on the circle should select the circle")
  (check-equal? (circ-pos (first (world-circles DRAG-SPEEDY-WORLD)))
                (make-position 200 300)
                "the circle should move according to the drag location")
  (check-equal? (circ-selected? (first (world-circles BUTTON-UP-AFTER-DRAG)))
                false
                "after button up event, the circle should be unselected")
  (check-equal? (circ-pos (first (world-circles DRAG-UNSELECTED)))
                (make-position (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2))
                "drag does not change the unselcted circles")
  (check-equal? (circ-after-mouse-event INIT-CIRCLE 0 0 "move") INIT-CIRCLE
                "irrelavent mouse event should not change the circle")
  (check-equal? (world-after-tick PAUSED-MOVING-WORLD) PAUSED-MOVING-WORLD
                "paused world should not change after a tick")
  (check-equal? (circle-after-tick (select-circle INIT-CIRCLE))
                (select-circle INIT-CIRCLE)
                "when selected, circle-after-tick should not change the circle")
  (check-equal? (circle-after-tick (new-circle 10 200 10 5))
                (new-circle RADIUS 205 -10 5))
  (check-equal? (circle-after-tick (new-circle 100 10 10 5))
                (new-circle 110 RADIUS 10 -5))
)