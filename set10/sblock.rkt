#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "interfaces.rkt")
(require "sets.rkt")
(provide SBlock%)

;;==============================================================================
;; CONSTANTS

(define SIDE-LENGTH 20)
(define HALF-SIDE-LENGTH (/ SIDE-LENGTH 2))

(define SB-IMAGE-NOT-SELECTED (square SIDE-LENGTH "outline" "green"))
(define SB-IMAGE-SELECTED     (square SIDE-LENGTH "outline" "red"))
;;==============================================================================
;; SBLOCK Class

;; Constructor Template
;; (new SBlock% [x Integer][y Integer] [selected? Boolean]
;;              [prev-mx Integer][prev-my Integer]
;;              [teammates ListOfSBlock] )
;; fields other than x & y are optional

;; A SBlock represents a block on the canvas.

(define SBlock%
  (class* object% (SBlock<%>)
    ;;==========================================================================
    (init-field x  ;; Number, x pixels of center from left
                y) ;; Number, y pixels of center from top
    ;;==========================================================================
    ;; selected? denotes if the block is selected, default value is false.
    ;; the value of selected? changes based on the mouse events.
    (init-field [selected? false])
    ;;==========================================================================
    ;; prev-mx denotes the x-component of previous mouse event which resulted to
    ;; the block getting selected similarly prev-my denotes the y-component
    ;; of previous mouse event which resulted to the block getting selected.
    ;; both are set to zero as the block is not selected by the mouse.
    (init-field [prev-mx 0] [prev-my 0])
    ;;==========================================================================
    ;; teammate is a set which contains the teammate that this block has
    (init-field [teammates empty])
    ;;==========================================================================
    (super-new)
    ;;==========================================================================
    ;; sblock-x  -> Int
    ;; GIVEN : No inputs are required
    ;; EXAMPLE : (send (new SBlock%) sblock-x)-> INIT-X
    ;; RETURNS : the x-component of the center of the block
    (define/public (sblock-x) x)
    ;;==========================================================================
    ;; sblock-y -> Int
    ;; GIVEN : No inputs are required
    ;; EXAMPLE : (send (new SBlock%) sblock-y)-> INIT-Y
    ;; RETURNS : the y-component of the center of the block
    (define/public (sblock-y) y)
    ;;==========================================================================
    ;; get-team -> Int
    ;; GIVEN : No inputs are required
    ;; EXAMPLE: (send (new SBlock%) get-team) -> empty
    ;; RETURNS : The teammates of the block
    (define/public (get-team) teammates)
    ;;==========================================================================
    ;; add-teammate : SBlock -> Void
    ;; GIVEN: a block
    ;; EFFECT: add given block into teammate list and teammates' teammate list
    ;; Haulting Measure: (- (length AllBlocks) (length TeammateOfGivenBlock))
    ;; Strategy:
    ;;   the Graph Recursion
    ;;   if given teammate already added to the list, terminate recursion
    ;;   if given teammate is this object itself, terminate recursion
    ;;   else add given teammate into teammate list and pass it to my teammates
    (define/public (add-teammate new-teammate)
      (if (or (member new-teammate teammates)
              (equal? new-teammate this)) 10
          (begin
            (set! teammates (cons new-teammate teammates))
            (for-each
             (lambda (tm) (send tm add-teammate new-teammate))
             teammates))))
    ;;==========================================================================
    ;; after-drag : Int Int -> Void
    ;; GIVEN: the position of mouse location
    ;; EFFECT: updates this block after drag event at the given location.
    ;; Strategy: case on whether the block is selected
    ;;           if selected, first move this block
    ;;           then move this block's teammates
    (define/public (after-drag mx my)
      (if selected?
          (let* ([dx (- mx prev-mx)]
                 [dy (- my prev-my)])
            (begin
              (set! prev-mx mx)
              (set! prev-my my)
              (send this move dx dy)
              (drag-teammates dx dy)))
          10))
    ;;==========================================================================
    ;; drag-teammates : Int Int -> Void
    ;; GIVEN: two integers representing the moving distance
    ;; EFFECT: move teammate according to the mouse position
    ;; Strategy: use HOF for-each on teammates
    (define (drag-teammates dx dy)
      (for-each
       (lambda (each-teammate) (send each-teammate move dx dy))
       teammates))
    ;;==========================================================================
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene to paint on
    ;; RETURNS: the given scene with this block painted on it
    ;; Strategy: case on whether the block is selected
    (define/public (add-to-scene scene)
      (if selected?
          (place-image SB-IMAGE-SELECTED x y scene)
          (place-image SB-IMAGE-NOT-SELECTED x y scene)))
    ;;==========================================================================
    ;; after-button-down : Int Int -> ListOfSBlock
    ;; GIVEN: the location of mouse down event
    ;; EFFECT: update this block after mouse down event at the given location
    ;; RETURNS: (list this) when (in-block? mx my), else return empty
    ;; Strategy: case on whether the mouse down location is in the block
    ;;           if inside, make this block selected, and return this block
    ;;           else return empty to tell SBlocks object that I'm not selected
    (define/public (after-button-down mx my)
      (if (in-block? mx my)
          (begin
            (set! selected? true)
            (set! prev-mx mx)
            (set! prev-my my)
            (list this))
          empty))
    ;;==========================================================================
    ;; intersected-blocks : ListOfSBlocks -> ListOfSBlocks
    ;; GIVEN: a list of unselected blocks
    ;; RETURNS: a list of blocks that intersects with this block
    ;;          but are not teammates
    ;; Strategy: use HOF filter on non-teammates
    (define/public (intersected-blocks lob)
      (let* ([non-teammates (set-diff lob teammates)])
        (filter
         intersect?
         non-teammates)))
    ;;==========================================================================
    ;; intersect? : SBlock -> Boolean
    ;; GIVEN: a block
    ;; RETURNS: true iff given block intersects with this block
    ;; Strategy: combine simpler functions 
    (define (intersect? b)
      (and (< (abs (- (send b sblock-x) x)) SIDE-LENGTH)
           (< (abs (- (send b sblock-y) y)) SIDE-LENGTH)))
    ;;==========================================================================
    ;; after-button-up : -> Void
    ;; GIVEN: mx and my represents the x and y components of the mouse
    ;;        co-ordinates respectively.
    ;; EFFECT: update this block to set selected false
    ;; EXAMPLE:
    ;;   (send (send (new SBlock%) after-button-up 120 120) sblock-x)-> INIT-X
    ;; Strategy: set the selected field to false
    (define/public (after-button-up mx my) (set! selected? false))
    ;;==========================================================================
    ;; after-tick : -> Void
    ;; GIVEN: no inputs
    ;; EFFECT: None
    ;; EXAMPLE: (send (send (new SBlock%) after-tick) sblock-x)-> INIT-X
    (define/public (after-tick) 19)
    ;;==========================================================================
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a keyevent 
    ;; EFFECT: None
    ;; EXAMPLE:
    ;;   (send (send (new SBlock%) after-key-event "a") sblock-x)-> INIT-X
    (define/public (after-key-event kev) 16)
    ;;==========================================================================
    ;; after-move:int int-> Void
    ;; GIVEN : mx and my represents the x and y components of the mouse
    ;;        co-ordinates respectively.
    ;; EFFECT: None
    ;; EXAMPLE: (send (send (new SBlock%) after-move 120 120) sblock-x)-> INIT-X
    (define/public (after-move mx my) 13)
    ;;==========================================================================
    ;; move : Int Int -> Void
    ;; GIVEN: delta x and delta y
    ;; EFFECT: move the x,y field by dx and dy
    ;; Strategy: update x and y according to the given dx and dy
    (define/public (move dx dy)
      (set! x (+ x dx))
      (set! y (+ y dy)))
    ;;==========================================================================
    ;;in-block? : PosInt PosInt -> Boolean
    ;;GIVEN : Two values represeting the mouse location in co-ordinate system
    ;;        where mx is the x-component and my is the y-component
    ;;RETURNS : Boolean true if the location of the mouse pointer falls within 
    ;;          the area of the SBlock.
    ;;EXAMPLE: (send (new SBlock%) in-block? INIT-X INIT-Y) -> true
    ;;STRATEGY: Combine Simpler Functions.
    (define (in-block? mx my)
      (and (< (abs (- mx x)) HALF-SIDE-LENGTH)
           (< (abs (- my y)) HALF-SIDE-LENGTH)))
    ))

;; TESTS


(define INIT-X 400)
(define INIT-Y 300)

(begin-for-test
  (local
    ((define block1 (new SBlock% [x INIT-X] [y INIT-Y]))
     (define block2 (new SBlock% [x INIT-X] [y INIT-Y]))
     )
    (check-equal? (send block1 sblock-x) INIT-X
                  "initial x value of block1 should be INIT-X")
    (check-equal? (send block1 sblock-y) INIT-Y
                  "initial x value of block1 should be INIT-X")
    (check-equal? (send block1 get-team) empty
                  "initially, block1 should have no blocks")
    (send block1 add-teammate block1)
    (check-equal? (send block1 get-team) empty
                  "when try to add self into team, self should not be added")
    (send block1 add-teammate block2)
    (check-equal? (length (send block1 get-team)) 1
                  "when block2 is added into team, block1 has 1 teammate")
    ))

