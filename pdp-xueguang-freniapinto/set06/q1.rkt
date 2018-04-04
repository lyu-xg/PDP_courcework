;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

(check-location "06" "q1.rkt")

(provide initial-world
         run
         world-after-mouse-event
         world-after-key-event
         world-to-trees
         tree-to-root
         tree-to-sons
         node-to-center
         node-to-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITION

;; NewNodeEvent is one of:
;; -- "c"
;; -- "s"

;; a ListOfX is one of:
;; -- empty
;; (cons X ListOfX)
;; TEMPLATE:
;; (define (lox-fn lox)
;;   (... (x-fn (first lox))
;;        (lox-fn (rest lox)))

;; a NodeSet of a Tree/ListOfTree represents all of the Node it consists

;; a Posn is a (make-posn Integer Integer)
;; TEMPLATE:
;; (define (posn-fn p)
;;   (... (posn-x p) (posn-y p))

(define-struct node (posn selected? circle? dx dy))
;; A Node is a (make-node Posn Boolean Boolean Integer Integer)
;; TEMPALTE:
;; (define (node-fn n)
;;    (... (posn-fn (node-posn n))
;;         (node-selected? n)
;;         (node-circle? n)
;;         (node-dx n)
;;         (node-dy n)))


(define-struct tree (node lot))
;; A Tree is a (make-tree Node ListOfTree)
;; TEMPLATE:
;; (define (tree-fn t)
;;   (... (node-fn (tree-node t)
;;        (lot-fn (tree-lot t)))


;; A ListOfTree is one of:
;; -- empty
;; -- (cons Tree ListOfTree)
;; TAMPLATE:
;; (define (lot-fn lon)
;;   (cond
;;      [(empty? lot) ...]
;;      [else (... (node-fn (frist lot))
;;                 (lot-fn (rest lot)))]

(define-struct world (lot))
;; A World is a (make-world ListOfTree)

;; A WorldState is a World

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTANTS

(define RADII 20)
;; Interp: the radius of the circle nodes

(define SIDE (* 2 RADII))
;; Interp: the side of the sqaure nodes

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
;; Interp: the size of the canvas is always 500*400 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPTER FUNCTIONS

;; node-x : Node -> Integer
;; RETURNS: the x value of the posn in the Node
;; Example: (node-x (make-node (make-posn 100 200) true true 0 0)) => 100
;; Strategy: use template for Node on n
(define (node-x n)
  (posn-x (node-posn n)))
;; node-y : Node -> Integer
;; RETURNS: the y value of the posn in the Node
;; Example: (node-y (make-node (make-posn 100 200) true true 0 0)) => 200
;; Strategy: use template for Node on n
(define (node-y n)
  (posn-y (node-posn n)))
;; TEST
(begin-for-test
  (check-equal? (node-x SELECTED-CIRCLE) 100
                "The x-position of the node is 100")
  
  (check-equal? (node-y SELECTED-CIRCLE) 200
                "The y-position of the node is 200"))


;; node-to-center : Node -> Posn
;; RETURNS: the center of the given node as it is to be displayed on the scene.
;; Strategy: use template for Node on n
(define (node-to-center n)
  (node-posn n))
;; TEST
(begin-for-test
  (check-equal? (node-to-center SELECTED-CIRCLE) (make-posn 100 200)
                "The poistion of the node is (100,200)"))

;; node-to-selected? : Node -> Boolean
;; RETURNS: true iff the given node is selected.
;; Strategy: use template for Node on n
(define (node-to-selected? n)
  (node-selected? n))
;; TEST
(begin-for-test
  (check-equal? (node-to-selected? SELECTED-CIRCLE) #true
                "The node should return true"))

;; tree-to-root : Tree -> Node
;; GIVEN: a tree
;; RETURNS: the node at the root of the tree
;; Strategy: use template for Tree on t
(define (tree-to-root t) (tree-node t))
;; TEST
(begin-for-test
  (check-equal? (tree-to-root TREE-SONS)
                (make-node (make-posn 150 160) true true 0 0)
                "This function should return the root node"))

;; tree-to-sons : Tree -> ListOfTree
;; GIVEN: a tree
;; RETURNS: the data associated with the immediate subtrees of the given tree. 
;; Strategy: use template for Tree on t
(define (tree-to-sons t) (tree-lot t))
;; TEST
(begin-for-test
  (check-equal? (tree-to-sons TREE-SONS)
                (list (make-tree (make-node (make-posn 175 180) false true 0 0) empty))
                "This function should return the children of the root node of the tree"))

;; euclidean-distance : Posn Posn -> number
;; GIVEN: two Posn
;; RETURNS: the euclidean distance of the two Posn
;; Strategy: use template for Posn
(define (euclidean-distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

;; TEST
(begin-for-test
  (check-equal? (euclidean-distance (make-posn 0 0) (make-posn 3 4))
                5 "the straight distance between 0,0 and 3,4 should be 5"))

;; in-circle? : Node Integer Integer -> Boolean
;; GIVEN: a circle Node and mouse positions as Integers
;; RETURNS: true iff the mouse positions is inside the circle
;; Strategy: use template for Node + combine simpler functions
(define (in-circle? n mx my)
  (>= RADII (euclidean-distance (node-posn n) (make-posn mx my))))

;; TEST
(begin-for-test
  (check-true
   (in-circle? (make-node (make-posn 100 200) false true 0 0) 101 201)
   "101,201 should be inside the circle")
  (check-false
   (in-circle? (make-node (make-posn 100 200) false true 0 0) 130 300)
   "130,300 should be outside the circle"))

;; in-square? : Node Integer Integer -> Boolean
;; GIVEN: a sqaure Node and mouse positions as Integers
;; RETURNS: true iff the mouse positions is inside the sqaure
;; Strategy: combine simpler functions
(define (in-square? n mx my)
  (and (< (abs (- mx (node-x n))) RADII)
       (< (abs (- my (node-y n))) RADII)))

;; TEST
(begin-for-test
  (check-equal?
   (in-square? SELECTED-CIRCLE 110 210) #true
   "(110,210) lies inside the square, should return true")
  (check-equal?
   (in-square? SELECTED-CIRCLE 160 260) #false
   "(160,260) lies outside the square, should return false"))


;; mouse-in? : Node Integer Integer -> Boolean
;; GIVEN: the node and mouse down position as Integers
;; RETURNS true iff the mouse down location is within the given node
;; Strategy: case on whether the node is a circle or a square
(define (mouse-in? n mx my)
  (cond
    [(node-circle? n) (in-circle? n mx my)]
    [else (in-square? n mx my)]))

;; TEST
(begin-for-test
  (check-equal? (mouse-in? SELECTED-CIRCLE 110 210) #true
                "The mouse lies inside the circle, hence true")
  (check-equal? (mouse-in? SQUARE-NODE 150 160) #false
                "The mouse lies outside the square, hence false"))


;; list-min : ListOfX -> X
;; RETURNS: the minimium of the list
;; Strategy: use HOF foldr on lox
(define (list-min lox)
  (inexact->exact
   (foldr
    ;; X X -> X
    ;; RETURNS: the min value of the two given X
    (lambda (x current) (min x current))
    +inf.0
    lox
    )))

;; TEST
(begin-for-test
  (check-equal? (list-min (list 1 4 6)) 1
                "the minium value of the list should be 1"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CORE FUNCTIONS

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: an initial world.  The given value is ignored.
;; Strategy: use definition of World
(define (initial-world val)
  (make-world empty))
;; TEST
(begin-for-test
  (check-equal? (initial-world 1) (make-world empty)
                "The initial world should be empty"))

;; run :  Any -> World
;; GIVEN: any value
;; EFFECT: runs a copy of an initial world
;; RETURNS: the final state of the world.  The given value is ignored.
;; Strategy: combine simpler functions
(define (run any)
  (big-bang
   (initial-world 0)
   (on-mouse world-after-mouse-event)
   (on-key world-after-key-event)
   (on-draw world-to-scene)))

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a World, a location, and a MouseEvent
;; RETURNS: the state of the world as it should be following the given mouse event at that location.
;; Strategy: case on the MouseEvent
(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev "button-down") (make-world
                                  (list-of-tree-after-down (world-lot w) mx my))]
    [(mouse=? mev "drag") (make-world
                           (list-of-tree-after-drag (world-lot w) mx my))]
    [(mouse=? mev "button-up") (make-world
                                (list-of-tree-after-up (world-lot w)))]
    [else w]))
;; TEST
(begin-for-test
  (check-equal? (world-after-mouse-event (make-world (list SELECTED-TREE)) 100 200 "button-up")
                (make-world (list UNSELECTED-TREE))
                "button up event should unselect all the nodes")
  (check-true (node-selected?
               (tree-node
                (first
                 (world-lot
                  (world-after-mouse-event
                   (make-world (list UNSELECTED-TREE)) 100 100 "button-down")))))
              "after key down inside the circle, the circle shuld be selected")
  (check-equal? (world-after-mouse-event
                 (make-world (list SELECTED-UNDRAGED-TREE-AT-100-200)) 110 210 "drag")
                (make-world (list DRAGED-TREE-BY-TEN))
                "after draging a cirlce at 100,200 by 10, the circle should end up at 110,210")
  (check-equal? (world-after-mouse-event (make-world empty) 0 0 "enter")
                (make-world empty)
                "irrelavent MouseEvents should not change the world"))

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a World and a key event
;; RETURNS: the state of the world as it should be following the given key event
;; Strategy: case on the KeyEvent
(define (world-after-key-event w key)
  (cond
    [(or (key=? key "s") (key=? key "c"))
     (world-after-new-node-event w key)]
    [(key=? key "d")
     (make-world (list-of-tree-after-d-event (world-lot w)))]
    [else w]))
;; TEST
(begin-for-test
  (check-equal? (world-after-key-event
                 (make-world (list
                              (make-tree UNSELECTED-CIRCLE
                                         (list (make-tree UNSELECTED-CIRCLE empty)
                                               (make-tree UNSELECTED-CIRCLE empty)
                                               (make-tree UNSELECTED-CIRCLE empty))))) "c")
                (make-world (list
                             (make-tree (make-node (make-posn 250 20) #false #true 0 0) '())
                             (make-tree (make-node (make-posn 100 100) #false #true 0 0)
                                        (list
                                         (make-tree (make-node (make-posn 100 100) #false #true 0 0) '())
                                         (make-tree (make-node (make-posn 100 100) #false #true 0 0) '())
                                         (make-tree (make-node (make-posn 100 100) #false #true 0 0) '())))))
                "Since no node is selected, a new circle node is created as new tree")
  
  (check-equal? (world-after-key-event (make-world TEST-DRAG-TREE) "k") (make-world TEST-DRAG-TREE)
                "Any other key event, should not cause any change")
  
  (check-equal? (world-after-key-event (make-world TEST-DRAG-TREE) "d")
                (make-world
                 (list
                  (make-tree
                   (make-node (make-posn 100 100) #false #true 0 0)
                   (list (make-tree (make-node (make-posn 120 130) #false #true 0 0) '())
                         (make-tree (make-node (make-posn 175 180) #false #true 0 0) '())))))
                "On the 'd' key event, the selected node must be deleted"))

;; world-to-scene : World -> Scene
;; GIVEN: A World and a key event
;; RETURNS: a scene which should represent the state of the world
;; Startegy: use HOF foldr on (world-lot w)
(define (world-to-scene w)
  (foldr
   ;; Tree Scene -> Scene
   ;; RETURNS: the Scene after drawing the tree on it
   (lambda (t canvas) (tree-to-scene t canvas))
   EMPTY-CANVAS
   (world-lot w)))
;; TEST
(begin-for-test
  (check-equal? (image-width (world-to-scene TEST-WORLD)) CANVAS-WIDTH
                "the width of the world scene should comply to the present constant")
  (check-equal? (world-to-scene (initial-world 0)) EMPTY-CANVAS
                "empty world should have nothing in it"))

;; world-to-trees : World -> ListOfTree
;; GIVEN: a World
;; RETURNS: a list of all the trees in the given world.
;; Strategy: use template of World on w
(define (world-to-trees w)
  (world-lot w))

;; tests
(begin-for-test
  (check-equal? (world-to-trees (make-world TEST-DRAG-TREE))
                TEST-DRAG-TREE
                "The function should return the list of trees"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; KEY EVENT FUNCTIONS

;; list-of-tree-after-d-event : ListOfTree -> ListOfTree
;; RETURNS: a list of Tree with the selected nodes deleted and its child
;;          become children of the parent of the deleted node
;; Strategy: use HOF foldr on lot
;; Halting measure: Size of NodeSet of lot
(define (list-of-tree-after-d-event lot)
  (foldr
   ;; Tree ListOfTree -> ListOfTree
   ;; RETURNS: a list of tree that includes the results of the given tree
   ;;          after the deletion event
   (lambda (t current-list)
     (append (result-of-delete (tree-after-d-event t)) current-list))
   empty
   lot))
;; TEST 
(begin-for-test
  (check-equal? (list-of-tree-after-d-event
                 (list (make-tree (make-node (make-posn 100 100) false true 0 0)
                   (list (make-tree (make-node (make-posn 120 130) false true 0 0) empty)
                         (make-tree (make-node (make-posn 150 160) true true 0 0) 
                                    (list (make-tree (make-node (make-posn 175 180) false true 0 0) empty)))))))
                (list (make-tree (make-node (make-posn 100 100) false true 0 0)
                   (list (make-tree (make-node (make-posn 120 130) false true 0 0) empty)
                         (make-tree (make-node (make-posn 175 180) false true 0 0) empty))))
                "the selected circle should be deleted after d event"))

;; result-of-delete : ListOfTree/Tree -> ListOfTree
;; GIVEN: a ListOfTree or a Tree
;; RETURNS: a list of tree representing to process result of tree-after-d-event
;;          in order to add the result sucessfully when calling list-of-tree-after-d-event
;; Strategy: case on whether the given argument is a list or a tree
(define (result-of-delete result)
  (if (list? result) result
      (cons result empty)))
;; TEST
(begin-for-test
  (check-equal? (result-of-delete TREE-SONS)
                (list TREE-SONS)
                "when given a tree, should return a list with the tree in it")
  (check-equal? (result-of-delete TEST-TREE)
                TEST-TREE
                "when given a list, should return what is given"))

;; tree-after-d-event : Tree -> ListOfTree/Tree
;; RETURNS: a Tree with the selected nodes deleted and its child
;;          become children of the parent of the deleted node
;; Strategy: case on whether the currect node is selected?
;; Halting measure: Size of NodeSet of t
(define (tree-after-d-event t)
  
  (if (node-selected? (tree-node t))
      (list-of-tree-after-d-event (tree-lot t))
      (make-tree (tree-node t) (list-of-tree-after-d-event (tree-lot t)))))
;; TEST
(begin-for-test
  (check-equal? (length (tree-after-d-event TREE-WITH-ONE-SON)) 1
                "when deleting TREE-WITH-ONE-SON, should return 1 son in a list"))

;; any-node-in-whole-list-of-tree-selected? : ListOfTree -> Boolean
;; GIVEN: a list of tree
;; RETURNS: true iff any node from the list of tree is selected
;; Strategy: Use HOF ormap on lot
;; Halting measure: Size of NodeSet of lot
(define (any-node-in-whole-list-of-tree-selected? lot)
  (ormap
   ;; Tree -> Boolean
   ;; GIVEN: a tree
   ;; RETURNS: true iff the node from the tree is selected
   (lambda(t) (any-node-in-whole-tree-selected? t))
   lot))
;; TEST
(begin-for-test
  (check-equal? (any-node-in-whole-list-of-tree-selected? TEST-DRAG-TREE) #true
                "The function should return true if any one of the nodes in the list is selected"))

;; any-node-in-whole-tree-selected? : Tree -> Boolean
;; GIVEN: a tree
;; RETURNS: true iff node from the tree is selected
;; Strategy:  use template for Tree on t
;; Halting measure: Size of NodeSet of t
(define (any-node-in-whole-tree-selected? t)
  (or (node-selected? (tree-node t))
      (any-node-in-whole-list-of-tree-selected? (tree-lot t))))
;; TEST
(begin-for-test
  (check-equal? (any-node-in-whole-tree-selected? TREE-SELECTED) true
                "for a selected tree, should return true"))

;; world-after-new-node-event : World NewNodeEvent -> Boolean
;; GIVEN: a World and node key event
;; RETURNS: the world state after the given NewNodeEvent
;; Strategy: case on whether there exists a selection in the (world-lot w)'s NodeSet
(define (world-after-new-node-event w key)
  (if (any-node-in-whole-list-of-tree-selected? (world-lot w))
      (make-world (list-of-tree-after-new-node-event (world-lot w) (key=? key "c")))
      (make-world (add-new-tree-to-list (world-lot w) (key=? key "c")))))
;; TEST
(begin-for-test
  (check-equal? (world-after-new-node-event (make-world TEST-DRAG-TREE) "s")
                (make-world
                 (list
                  (make-tree
                   (make-node (make-posn 100 100) #false #true 0 0)
                   (list
                    (make-tree (make-node (make-posn 120 130) #false #true 0 0) '())
                    (make-tree
                     (make-node (make-posn 150 160) #true #true 0 0)
                     (list (make-tree (make-node (make-posn 115 220) #false #false 0 0) '())
                           (make-tree (make-node (make-posn 175 180) #false #true 0 0) '())))))))
                "On 's' key event, if any node selected, the node becomes parent to new square node"))

;; add-new-tree-to-list : ListOfTree Boolean-> ListOfTree
;; GIVEN: a list of tree and the new son shape
;; RETURNS: a list of tree with new nodes 
;; Strategy: use definition of Tree and Node
(define (add-new-tree-to-list lot circle?)
  (cons (make-tree
         (make-node
          (make-posn (/ CANVAS-WIDTH 2) RADII ) #false circle? 0 0) empty) lot))
;; TEST
(begin-for-test
  (check-equal? (add-new-tree-to-list empty false)
                (list (make-tree
                       (make-node (make-posn 250 20) #false #false 0 0) empty))
                "The function should create a new tree with node"))

;; list-of-tree-after-new-node-event : ListOfTree Boolean -> ListOfTree
;; GIVEN: a list of tree and if the new node is circle or a square
;; RETURNS: list of tree with a new son at the selected node
;; Strategy: Use HOF map on lot
;; Halting measure: Size of NodeSet of lot
(define (list-of-tree-after-new-node-event lot circle?)
  (map
   ;; Tree  -> Tree
   ;; GIVEN: a tree and if the new node is circle or a square
   ;; RETURNS: The given tree after new node event
   (lambda (t) (tree-after-new-node-event t circle?))
   lot))
;; TEST
(begin-for-test
  (check-equal? (list-of-tree-after-new-node-event TEST-DRAG-TREE true)
                (list
                 (make-tree
                  (make-node (make-posn 100 100) #false #true 0 0)
                  (list
                   (make-tree (make-node (make-posn 120 130) #false #true 0 0) '())
                   (make-tree
                    (make-node (make-posn 150 160) #true #true 0 0)
                    (list (make-tree (make-node (make-posn 115 220) #false #true 0 0) '())
                          (make-tree (make-node (make-posn 175 180) #false #true 0 0) '()))))))
                "The function creates a new circle node at the selected(parent) node"))

;; tree-after-new-node-event : Tree -> Tree
;; GIVEN: a tree
;; RETURNS: a tree with new nodes
;; Strategy: combine simpler functions
;; Halting measure: Size of NodeSet of t
(define (tree-after-new-node-event t circle?)
  (cond
    [(node-selected? (tree-node t)) (new-son t circle?)]
    [else (make-tree (tree-node t) (list-of-tree-after-new-node-event (tree-lot t) circle?))]))
;; TEST
(begin-for-test
  (check-equal? (tree-after-new-node-event TREE-SONS false)
                (make-tree
                 (make-node (make-posn 150 160) #true #true 0 0)
                 (list
                  (make-tree (make-node (make-posn 115 220) #false #false 0 0) '())
                  (make-tree (make-node (make-posn 175 180) #false #true 0 0) '())))
                "This function creates a new square node at (115,220"))

;; new-son : Tree Boolean -> Tree
;; GIVEN: a tree and boolean representing the node is a circle or a square
;; RETURNS: adds new sons to the node of the tree
;; Strategy: combine simpler functions
(define (new-son t circle?)
  (make-tree (tree-node t)
             (cons (make-tree
                    (make-node (new-son-position t) false circle?
                               (node-dx (tree-node t)) (node-dy (tree-node t))) empty)
                   (tree-lot t))))
;; TEST
(begin-for-test
  (check-equal? (new-son TREE-SONS true)
                (make-tree
                 (make-node (make-posn 150 160) #true #true 0 0)
                 (list
                  (make-tree (make-node (make-posn 115 220) #false #true 0 0) '())
                  (make-tree (make-node (make-posn 175 180) #false #true 0 0) '())))
                "The function creates a new circle node at (115,220)")
  (check-equal? (new-son TREE-SONS false)
                (make-tree
                 (make-node (make-posn 150 160) #true #true 0 0)
                 (list
                  (make-tree (make-node (make-posn 115 220) #false #false 0 0) '())
                  (make-tree (make-node (make-posn 175 180) #false #true 0 0) '())))
                "This function creates a new square node at (115,220"))

;; new-son-position : Tree -> Posn
;; GIVEN: a tree
;; RETURNS: the position of where a new son should be
;; Strategy: use template for Tree on t
(define (new-son-position t)
  (make-posn (new-son-x t)
             (+ (node-y (tree-node t)) (* 3 RADII))))
;; Test
(begin-for-test
  (check-equal? (new-son-position TREE-SONS) (make-posn 115 220)
                "The position of the new son will be at (115,220)"))

;; new-son-x : Tree -> Integer
;; RETURNS: the x coordinate of where a new son should be
;; Strategy: case on whether the given tree has any son
(define (new-son-x t)
  (if (empty? (list-of-tree-x (tree-lot t)))
      (node-x (tree-node t))
      (- (list-min (list-of-tree-x (tree-lot t))) (* 3 RADII))))
;; TEST
(begin-for-test
  (check-equal? (new-son-x TREE-SONS) 115
                "The new node should be a circle whose center has an x-coordinate that is 3 radii to the left of the center of the currently leftmost son")
  (check-equal? (new-son-x
                 (make-tree (make-node (make-posn 175 180)
                                       false true 0 0) empty)) 175
                "The new node should be a circle directly below parent when the parent has no sons"))


;; list-of-tree-x: ListOfTree -> ListOfInteger
;; RETURNS: the x values of the list of tree's nodes
;; Strategy: use HOF map on t
;; Halting measure: Size of NodeSet of lot
(define (list-of-tree-x lot)
  (map
   ;; Tree -> Integer
   ;; RETURNS: the x coordinate of the given Tree
   (lambda (t) (node-x (tree-node t)))
   lot))
;; TEST
(begin-for-test
  (check-equal? (list-of-tree-x (list (make-tree (make-node (make-posn 120 130) false true 0 0) empty)
                                      (make-tree (make-node (make-posn 150 160) true true 0 0) empty)))
                (list 120 150)
                "The function should return the list of x-positions"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MOUSE EVENT FUNCTIONS


;==========================
;; list-of-tree-after-up : ListOfTree -> ListOfTree
;; RETURNS: the list of tree after mouse up event
;; Strategy: use HOF map on lot
;; Halting measure: Size of NodeSet of lot
(define (list-of-tree-after-up lot)
  (map tree-after-up lot))
;; TEST
(begin-for-test
  (check-equal? (list-of-tree-after-up (list SELECTED-TREE))
                (list UNSELECTED-TREE)
                "list-of-tree-after-up should unselect all the nodes"))

;; tree-after-up : Tree -> Tree
;; RETURNS: the Tree after mouse up event
;; Strategy: use definition for Tree on t
;; Halting measure: Size of NodeSet of t
(define (tree-after-up t)
  (make-tree (node-after-up (tree-node t))
             (list-of-tree-after-up (tree-lot t))))
;; TEST
(begin-for-test
  (check-equal? (tree-after-up  SELECTED-TREE)
                UNSELECTED-TREE
                "tree-after-up should unselect all the nodes"))


;; node-after-up : Node -> Node
;; RETURNS: the state of the Node with the selected? field changed to false
;; Strategy: use template for Node on n
(define (node-after-up n)
  (make-node (node-posn n) false (node-circle? n) 0 0))
;; TEST
(begin-for-test
  (check-equal? (node-after-up SELECTED-CIRCLE2)
                UNSELECTED-CIRCLE2
                "circle should be unselected after button up event"))

;==========================
;; list-of-tree-after-down : ListOfTree -> ListOfTree
;; RETURNS: the list of tree after mouse down event
;; Strategy: use HOF map on lot
;; Halting measure: Size of NodeSet of lot
(define (list-of-tree-after-down lot mx my)
  (map
   ;; Tree Integer Integer -> Tree
   ;; RETURNS: the state of the tree after mouse down event at given location
   (lambda (t) (tree-after-down t mx my))
   lot))
;; TEST
(begin-for-test
  (check-true (node-selected?
               (tree-node
                (first (list-of-tree-after-down (list UNSELECTED-TREE) 100 100))))
              "after key down inside the circle, the circle shuld be selected"))

;; tree-after-down : Tree Integer Integer -> Tree
;; RETURNS: the state of the tree after mouse donw event
;; Strategy: use definition for Tree on t
;; Halting measure: Size of NodeSet of t
(define (tree-after-down t mx my)
  (make-tree (node-after-down (tree-node t) mx my)
             (list-of-tree-after-down (tree-lot t) mx my)))
;; TEST
(begin-for-test
  (check-true (node-selected?
               (tree-node
                (tree-after-down UNSELECTED-TREE 100 100)))
              "after key down inside the circle, the circle shuld be selected"))

;; node-after-down : Node Integer Integer -> Node
;; RETURNS: the state of the Node after button down event
;; Strategy: use template for Node on n
(define (node-after-down n mx my)
  (make-node (node-posn n) (mouse-in? n mx my) (node-circle? n)
             mx my))
;; TEST
(begin-for-test
  (check-true (node-selected? (node-after-down UNSELECTED-CIRCLE 100 100))
                "after key down in the circle, the circle should be selected"))

;==========================
;; list-of-tree-after-drag: ListOfTree Integer Integer -> ListOfTree
;; RETURN: the list of Tree after given mouse event at given position
;; Strategy: use HOF map on lot
(define (list-of-tree-after-drag lot mx my)
  (map
   ;; Tree  -> Tree
   ;; RETURNS: The given tree after mouse drag event
   (lambda (t) (tree-after-drag t mx my))
   lot))
;; TEST
(begin-for-test
  (check-equal? (list-of-tree-after-drag (list SELECTED-UNDRAGED-TREE-AT-100-200) 110 210)
                (list DRAGED-TREE-BY-TEN)
                "after draging a cirlce at 100,200 by 10, the circle should end up at 110,210"))

;; tree-after-drag : Tree Integer Integer -> Tree
;; RETURNS: the state of the given Tree after mouse drag event at given position
;; Strategy: case on whether the (tree-node t) is selected or not
;; Halting measure: Size of NodeSet of t
(define (tree-after-drag t mx my)
  (if (node-selected? (tree-node t))
      (make-tree (node-apply-drag (tree-node t) mx my)
                 (list-of-tree-apply-drag (tree-lot t) mx my))
      (make-tree (tree-node t)
                 (list-of-tree-after-drag (tree-lot t) mx my))))
;; TEST
(begin-for-test
  (check-equal? (tree-after-drag (make-tree UNSELECTED-CIRCLE (list SELECTED-UNDRAGED-TREE-AT-100-200)) 110 210)
                (make-tree UNSELECTED-CIRCLE (list DRAGED-TREE-BY-TEN))
                "after draging a cirlce at 100,200 by 10, the circle should end up at 110,210"))


;; list-of-tree-apply-drag: ListOfTree Integer Integer -> ListOfTree
;; RETURNS: the given list where every Tree applied
;           the mouse event effect at given mouse position
;; Strategy: use HOF map on lot
;; Halting measure: Size of NodeSet of lot
(define (list-of-tree-apply-drag lot mx my )
  (map
   ;; Tree -> Tree
   (lambda (t) (tree-apply-drag t mx my ))
   lot))
;; TEST
(begin-for-test
  (check-equal? (list-of-tree-apply-drag (list SELECTED-UNDRAGED-TREE-AT-100-200) 110 210)
                (list DRAGED-TREE-BY-TEN)
                "after draging a cirlce at 100,200 by 10, the circle should end up at 110,210"))

;; tree-apply-drag : Tree Integer Integer -> Tree
;; RETURNS: the given Tree applied the given mouse event effect at given position
;; Strategy: use template for Tree on t
;; Halting measure: Size of NodeSet of t
(define (tree-apply-drag t mx my )
  (make-tree (node-apply-drag (tree-node t) mx my)
             (list-of-tree-apply-drag (tree-lot t) mx my)))
(begin-for-test
  (check-equal? (tree-apply-drag SELECTED-UNDRAGED-TREE-AT-100-200 110 210)
                 DRAGED-TREE-BY-TEN
                "after draging a cirlce at 100,200 by 10, the circle should end up at 110,210"))

;; node-apply-drag : Node Integer Integer -> Node
;; RETURNS: the state of the given Node after mouse drag event at given position
;; Strategy: use template for Node on n
(define (node-apply-drag n mx my)
  (make-node (make-posn (- (+ (node-x n) mx) (node-dx n))
                        (- (+ (node-y n) my) (node-dy n)))
             (node-selected? n) (node-circle? n) mx my))
;; TEST
(begin-for-test
  (check-equal? (node-apply-drag (make-node (make-posn 100 200) true true 100 200) 110 210)
                (make-node (make-posn 110 210) true true 110 210)
                "after draging a cirlce at 100,200 by 10, the circle should end up at 110,210"))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TO SCENE FUNCTIONS


;; list-of-tree-to-scene : ListOfTree Posn Scene -> Scene
;; GIVEN: A ListOfTree and a Posn representing the father position of given trees
;;        and a Scene to paint on
;; RETURNS : the Scene representing the given ListOfTree
;; Strategy: use HOF foldr on lot
;; Halting measure: Size of NodeSet of lot
(define (list-of-tree-to-scene lot father-posn canvas)
  (foldr
   ;; Tree Scene -> Scene
   ;; RETURNS: the Scene with the given Tree drawed on it
   (lambda (t canvas) (draw-line-and-tree t father-posn canvas))
   canvas
   lot))
;; TEST
(begin-for-test (list-of-tree-to-scene TEST-LOT (make-posn 0 0) EMPTY-CANVAS)
                (list-of-tree-to-scene SELECTED-LOT (make-posn 0 0) EMPTY-CANVAS)
                "the two lists should be identical after drawing when given same father location")

;; draw-line-and-tree : Tree Pson Scene -> Scene
;; RETURNS: A Scene after drawing the given Tree and a line to the Tree
;; Strategy: Use template for Tree on t
;; Halting measure: Size of NodeSet of t
(define (draw-line-and-tree t father-posn canvas)
  (scene+line (tree-to-scene t canvas)
              (node-x (tree-node t)) (node-y (tree-node t))
              (posn-x father-posn) (posn-y father-posn) "blue"))
;; TEST
(begin-for-test
  (check-equal? (draw-line-and-tree TREE-SELECTED (make-posn 10 10) EMPTY-CANVAS)
                (draw-line-and-tree TREE-WITH-ONE-SON (make-posn 10 10) EMPTY-CANVAS)
                "two trees should be identical after drawing with same father location"))

;; tree-to-scene : Tree Scene -> Scene
;; RETURNS: the Scene representing the given Tree and its children
;; Strategy: use template for Tree on t
;; Halting measure: Size of NodeSet of t
(define (tree-to-scene t canvas)
  (place-image
   (node-to-image (tree-node t))
   (node-x (tree-node t)) (node-y (tree-node t))
   (list-of-tree-to-scene (tree-lot t) (node-posn (tree-node t)) canvas)))
;; TEST
(begin-for-test
  (check-equal? (tree-to-scene TREE-SELECTED EMPTY-CANVAS)
                (tree-to-scene TREE-WITH-ONE-SON EMPTY-CANVAS)
                "two trees should be identical after drawing"))
                               
;; node-to-image : Node -> Image
;; RETURNS: a Image representing the Node
;; Strategy: case on whether the node is a cirlce or a square
(define (node-to-image n)
  ((if (node-circle? n) circle square)
   (if (node-circle? n) RADII SIDE)
   (if (node-selected? n) "solid" "outline") "green"))
;; TEST
(begin-for-test
  (check-equal? (node-to-image UNSELECTED-CIRCLE)
                (node-to-image UNSELECTED-CIRCLE2)
                "UNSELECTED-CIRCLE should be same after draw as UNSELECTED-CIRCLE2")
  (check-equal? (image-width (node-to-image SELECTED-SQUARE))
                (image-height (node-to-image SELECTED-SQUARE))
                "The width should be equal to height after drawing a sqaure"))


;; TEST CONSTANTS

(define UNSELECTED-CIRCLE (make-node (make-posn 100 100) false true 0 0))
(define SELECTED-CIRCLE (make-node (make-posn 100 200) true true 0 0))
(define UNSELECTED-CIRCLE2 (make-node (make-posn 100 100) false true 0 0))
(define SELECTED-CIRCLE2 (make-node (make-posn 100 100) true true 0 0))
(define SELECTED-SQUARE (make-node (make-posn 100 100 ) false false 0 0))
(define SELECTED-TREE (make-tree SELECTED-CIRCLE2 empty))
(define UNSELECTED-TREE (make-tree UNSELECTED-CIRCLE2 empty))

(define SELECTED-UNDRAGED-TREE-AT-100-200 (make-tree (make-node (make-posn 100 200) true true 100 200) empty))
(define DRAGED-TREE-BY-TEN (make-tree (make-node (make-posn 110 210) true true 110 210) empty)) 


(define SQUARE-NODE (make-node (make-posn 100 200) true false 0 0))


(define TREE-SONS (make-tree (make-node (make-posn 150 160) true true 0 0)
                             (list (make-tree (make-node (make-posn 175 180) false true 0 0) empty))))
(define TREE-WITH-ONE-SON TREE-SONS)
(define TREE-SELECTED TREE-SONS)
(define TEST-TREE
  (list (make-tree UNSELECTED-CIRCLE
                   (list (make-tree UNSELECTED-CIRCLE (list (make-tree UNSELECTED-CIRCLE empty)
                                                            (make-tree UNSELECTED-CIRCLE empty)
                                                            (make-tree UNSELECTED-CIRCLE empty)))
                         (make-tree SELECTED-CIRCLE (list (make-tree UNSELECTED-CIRCLE empty)
                                                          (make-tree UNSELECTED-CIRCLE empty)))))))
(define TEST-LOT TEST-TREE)
(define SELECTED-LOT TEST-TREE)
(define TEST-DRAG-TREE
  (list (make-tree (make-node (make-posn 100 100) false true 0 0)
                   (list (make-tree (make-node (make-posn 120 130) false true 0 0) empty)
                         (make-tree (make-node (make-posn 150 160) true true 0 0) 
                                    (list (make-tree (make-node (make-posn 175 180) false true 0 0) empty)))))))


(define TEST-WORLD (make-world TEST-TREE))
  
  
  
  
  
  







