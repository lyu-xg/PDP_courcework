#lang racket
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(require "interfaces.rkt")
(require "constants.rkt")
(require "sblock.rkt")
(require "sets.rkt")
(provide SBlocks%)

;;==============================================================================
;; CONSTANTS

(define NEW-BLOCK-EVENT "b")

;;==============================================================================
;; SBLOCKS Class

;; Constructor Template
;; (new SBlocks% [blocks ListOfSBlock] [selected ListOfSBlock])
;; all fields are optional

;; A SBlock represents a series of blocks on the canvas.

(define SBlocks%
  (class* object% (Metatoy<%>) 
    ;===========================================================================    
    ;; blocks is a SetOfSBlock that contains all the blocks 
    (init-field [blocks empty])
    ;;==========================================================================
    ;; selected is also a SetOfSBlock but only contain ones that are selected
    (init-field [selected empty])
    ;;==========================================================================
    ;; mx,my are stored mouse locations, used when creating new blocks
    (init-field [prev-mx INIT-X] [prev-my INIT-Y])
    ;===========================================================================    
    (super-new)
    ;===========================================================================    
    ;; after-tick : -> Void
    ;; GIVEN: No arguements
    ;; EFFECT: updates this SBlocks to the state it should have following a tick
    ;; DETAILS: SBlocks' states does not change when time passes
    (define/public (after-tick) 43)
    ;;==========================================================================
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: the given scene with this sblocks painted on it
    ;; Strategy: use HOF foldr on blocks
    (define/public (add-to-scene scene)
      (foldr
       (lambda (block scene)
         (send block add-to-scene scene))
       scene
       blocks))
    ;;==========================================================================    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a keyevent
    ;; EFFECT: update the this sblocks by added a new block when given
    ;;         a new block event
    ;; Strategy: case on whether the given event is new block event or not
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-BLOCK-EVENT)
         (set! blocks (cons (new SBlock% [x prev-mx] [y prev-my]) blocks))]
        [else 43]))
    ;;==========================================================================
    ;; get-toys : -> ListOfToy
    ;; RETURNS: the list of block this SBlocks has
    ;; Example: (send (new SBlocks% [blocks lob])) -> lob
    ;; Strategy: return a field value of this object
    (define/public (get-toys) blocks)
    ;;==========================================================================   
    ;; after-button-down : Int Int -> Void
    ;; GIVEN: the location of button down event
    ;; EFFECT: update this sblocks according to location of button down event
    ;; Strategy: collect sellected blocks using HOF foldr on the blocks,
    ;;           stored the collected list in the selected field
    (define/public (after-button-down mx my)
      (set! prev-mx mx)
      (set! prev-my my)
      (set! selected
            (foldr
             ;; SBlock ListOfSBlock -> ListOfSBlock
             ;; RETURNS: the unchanged list if the block should not be selected,
             ;;          the list with block added if block should be selected
             (lambda (block sofar)
               (append sofar (send block after-button-down mx my)))
             empty blocks)))
    ;;==========================================================================
    ;; after-button-up : Int Int -> Void
    ;; GIVEN: the location of button up event
    ;; EFFECT: update all the blocks and all of them unselected
    ;; Strategy: use HOF for-each on blocks
    (define/public (after-button-up mx my)
      (set! prev-mx mx)
      (set! prev-my my)
      (for-each
         (lambda (block) (send block after-button-up mx my))
         blocks))
    ;;==========================================================================  
    ;; after-drag : Int Int -> Void
    ;; GIVEN: the location of mouse drag event
    ;; EFFECT: move the selected blocks and their teammates
    ;; Strategy: case on whether there is any selected blocks
    (define/public (after-drag mx my)
      (set! prev-mx mx)
      (set! prev-my my)
      (if (empty? selected) 12
          (begin 
            (for-each
             find-new-teammates
             selected)
            (send (first selected) after-drag mx my))))
    ;;==========================================================================
    ;; after-move : Int Int -> Void
    ;; GIVEN: the locatin of mouse move event
    ;; EFFECT: None
    (define/public (after-move mx my)
      (set! prev-mx mx)
      (set! prev-my my))
    ;;==========================================================================
    ;; find-new-teammates : SBlock -> Void
    ;; GIVEN: a block
    ;; WHERE: the given block is selected
    ;; EFFECT: find teammates for the selected block, and make them teammates
    ;; Strategy: first find blocks that are intersected with the given block 
    ;;           then use HOF for-each on intersected to make them teammates
    (define (find-new-teammates block)
      (let ([intersected
             (send block intersected-blocks (set-diff blocks selected))])
        (for-each
         (lambda (teammate) (make-teammates teammate block))
         intersected)))
    ;;==========================================================================
    ;; make-teammates: SBlock SBlock -> Void
    ;; GIVEN: two blocks
    ;; EFFECT: add the given blocks into each others' teams
    ;; Strategy: combine simpler functions
    (define (make-teammates b1 b2)
      (add-all-teammates b1 b2)
      (add-all-teammates b2 b1))
    ;;==========================================================================
    ;; add-all-teammates: SBlock SBlock -> Void
    ;; GIVEN: two blocks b1 and b2
    ;; EFFECT: add all teammates from b1 to b2
    ;; Strategy: first add b1 into b2's team
    ;;           then add all of b1's teammates into b2's team
    (define (add-all-teammates b1 b2)
      (send b2 add-teammate b1)
      (for-each
       (lambda (b) (send b2 add-teammate b))
       (send b1 get-team)))
    ))

