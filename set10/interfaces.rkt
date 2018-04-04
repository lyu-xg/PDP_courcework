#lang racket
(require "WidgetWorks.rkt")
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)




(provide
 Toy<%>
 Metatoy<%>
 SBlock<%>
 euclidean-distance
 )

(define Metatoy<%>
  (interface (SWidget<%>)
    ;;get-toys: ->ListOfToys
    ;;RETURNS: The list of toys present in the metatoy.
    get-toys

    
  ))

(define Toy<%> 
  (interface
  
   ;; The interface Toy<%> inherits from the interface Widget<%>.
   ;; This means that any class that implements Toy<%> must implement
   ;; all the methods from Widget<%> plus all the methods defined here.
   (SWidget<%>)


    ;; Note: the Widgets of the space-invader-examples don't respond
    ;; to mouse "move" events, but some of our toys do.  So we add an
    ;; after-move method to the interface.

    ;;  Int Int -> Toy
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    

 
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

;===============================================================================
(define SBlock<%>
  (interface (SWidget<%>)
    
    ;;get-team : -> ListOfSBlock
    ;;RETURNS: the teammates of this sblock
    get-team
    
    ;;add-teammate: SBlock -> Void
    ;;EFFECT: adds the given sblock to this block's team
    add-teammate
    
    ;;sblock-x : -> Integer
    ;;sblock-y : -> Integer
    ;;RETURNS: the x or y coordinates of this sblock
    sblock-x
    sblock-y

    ;; move : Int Int -> Void
    ;; GIVEN: delta x and delta y
    ;; EFFECT: move the x,y field by dx and dy
    move

    ;; intersected-blocks : ListOfSBlocks -> ListOfSBlocks
    ;; GIVEN: a list of unselected blocks
    ;; RETURNS: a list of blocks that intersects with this block
    ;;          but are not teammates
    intersected-blocks
    ))
;;==============================================================================
(define (euclidean-distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))