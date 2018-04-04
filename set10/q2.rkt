#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "interfaces.rkt")
(require "constants.rkt")
(require "sblocks.rkt")
(require "sblock.rkt")
(provide SBlock<%> make-block cubelets-init) 

;; Interactive Interface run command: (send (cubelets-init) run .1)


;; cubelets-init : -> Container
;; GIVEN: no arguments
;; RETURNS: a Container, initially with no blocks, which when run, will
;;    run in a 600x500 canvas and process the events in the description 
(define (cubelets-init)
  (let ([container (container-init CANVAS-WIDTH CANVAS-HEIGHT)])
    (begin
      (send container add-stateful-widget (new SBlocks%))
      container)))

;; make-block : NonNegInt NonNegInt ListOfSBlock -> SBlock
;; GIVEN: an x and y position, and a list of blocks
;; WHERE: the list of blocks is the list of blocks already on the playground.
;; EXAMPLE: see test cases below
;; RETURNS: a new block, at the given position, with no teammates
(define (make-block x y lob) (new SBlock% [x x] [y y]))

;; TESTS

(begin-for-test
  (local
    ((define block1 (make-block 10 20 empty))
     (define blocks1 (cubelets-init))
     (define blocks2 (send blocks1 add-stateful-widget block1)))
    (check-equal? (send block1 sblock-x) 10
                  "the initial x of block1 should be 10")))