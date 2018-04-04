;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

(check-location "08" "q1.rkt")

(provide make-def
         make-varexp
         make-appexp
         any-loops?
         )
;===============================================================================
;; BORING OLD DATA DEFINITION

;; A Program is a ListOfDefinition.
;; A Variable is a Symbol.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.
;; TEMPALTE
;; def-fn : Definition -> ??
#;
(define (def-fn d)
  (... (def-name d)
       (lov-fn (def-args d))
       (exp-fn (def-body d))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct varexp (name))
(define-struct appexp (fn args))
;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en
;; TEMPLATE
;; exp-fn : Exp -> ??
#;
(define (exp-fn e)
  (cond
    [(varexp? e) (... (var-exp-name e))]
    [(appexp? e) (... (app-fn (app-exp-fn e) (app-exp-args e)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A ListOfX is one of
;; -- empty
;; -- (cons X ListOfX)
;; TEMPLATE
;; lox-fn : X -> ??
#;
(define (lox-fn lox)
  (if (empty? lox) ...
      (... (x-fn (first lox))
           (lox-fn (rest lox)))))
;===============================================================================
;; INTERESTING NEWLY ADDED DATA DEFINITION

(define-struct node (name lon))
;; A Node is a (make-node Symbol ListOfSymbol)
;; INTERPRETATION
;; name is the function name
;; lon is the list of functions in the body of that function
;; TEMPLATE
#;
(define (node-fn n)
  (... (node-name n)
       (lon-fn (lon n))))
;===============================================================================
;===============================================================================
;; CORE FUNCTIONS

;; any-loops? : Program -> Boolean
;; GIVEN: a valid SGS program p
;; RETURNS: true iff there is some function f in p that calls itself
;;          either directly or indirectly, as in the example above.
;; Strategy: Use HOF ormap on program
(define (any-loops? program)
  (let [(policy (get-all-edges program))]
  (ormap
   ;Symbol -> Boolean
   ;RETURNS: true iff the given function name has a loop
   (lambda (fn-name) (loops? empty (list fn-name) policy))
   (get-all-fn-name program))))
;; TEST
(begin-for-test
  (check-true (any-loops? some-loops)
              "some-loops has functions that call itself"))
;===============================================================================
;; loops? : SetOfSymbol SetOfSymbol ListOfNode -> Boolean
;; GIVEN: a set of symbol visited, a set of symbol workset
;;        a list of node representing the program in a graph
;; RETURNS: true iff there is some function f in p that calls itself
;;          either directly or indirectly (loop is found)
;; Strategy: if sucessors contains symbol that is already visited, return true;
;;           else if there are no more sucessors, return false
;;           else recur on sucessors as workset 
;; Halting Measure: The length of unexplored nodes in the given list of nodes
(define (loops? visited workset policy)
  (let [(sucessors (all-successors workset policy))]
    (cond [(intersect? visited sucessors policy) true]
          [(empty? sucessors) false]
          [else (loops? (set-union visited workset)
                        sucessors
                        policy)])))
;;TESTS
(begin-for-test
  (check-equal? (intersect? (list 'no-loop) (list 'f1) NODES-TEST) #false
                "the function does not result in a loop"))
;===============================================================================
;; intersect? : SetOfSymbol SetOfSymbol ListOfNode -> Boolean
;; GIVEN: two sets of symbol, and a list of nodes representing policy graph
;; RETURNS: true iff the sucessors contains symbol that is already visited
;; Strategy: Use HOF ormap on s1
(define (intersect? s1 s2 policy)
  (ormap
   ;Symbol -> Boolean
   ;RETURNS: true if the element belongs in the set and has successors
   (lambda (x)
     (and (member? x s2)
          (not (empty? (successor x policy)))))
   s1))
;; TEST
(begin-for-test
  (check-equal? (intersect? (list 'f1) (list 'no-loop) NODES-TEST) #false
                "The successor of f1 is no-loop, hence false"))
;===============================================================================
;; all-successors : SetOfSymbol ListOfNodes -> SetOfSymbol
;; GIVEN: a set of symbol, and list of nodes representing policy
;; RETURNS: a set of all immediate children of given symbol
;; Strategy: Use HOF foldr on workset
(define (all-successors workset edges)
  (foldr
   ;; Symbol -> SetOfSymbol
   ;; RETURNS: the set of symbol with each-symbol's child added
   (lambda (each-symbol sofar)
     (set-union (successor each-symbol edges) sofar))
   empty
   workset))

;; TEST
(begin-for-test
  (check-equal? (all-successors (list 'f2 'f3) NODES-TEST)
                (list 'f1 'f4)
                "The functions that the list of function calls"))
;===============================================================================
;; successor : Symbol ListOfNodes -> SetOfSymbol
;; GIVEN: a symbol s and a list of nodes representing policy in a graph
;; RETURNS: the seccessor of given symbol through look up the given policy
;; Strategy: use HOF filter on policy
(define (successor s policy)
  (node-lon
   (first (filter
     ; Node -> Boolean
     ; RETURNS: true iff the Symbol is the name of the node
     (lambda (n)
       (equal? (node-name n) s)) policy))))
;;TEST
(begin-for-test
  (check-equal? (successor 'f3 NODES-TEST) (list 'f1 'f4)
                "The calls of f3 are linked to f1 and f4"))
;===============================================================================
;===============================================================================
;; get-all-edges : Program -> ListOfNodes
;; GIVEN: a program
;; RETURNS: a list of nodes representing the name of function
;;          and associated function call of that function
;; Strategy: Use HOF foldr on program
(define (get-all-edges program)
  (foldr
   ; Defintion -> ListOfNodes
   ; RETURNS: the list of nodes associated with the functions
   (lambda (p sofar)
     (cons (make-node (def-name p)
                      (get-edges (list (def-body p)))) sofar))
   empty
   program))
;; TEST
(begin-for-test
  (check-equal? (get-all-edges some-loops)
                NODES-TEST
                "The function generates a list of nodes"))
;===============================================================================
;; get-edges : ListOfExpression -> ListOfSymbol
;; GIVEN: a list of exp
;; RETURNS: the list of all function in the list of exp
;; Strategy: Use HOF foldr on loe
;; Halting Measure: length of all decendants of exp
(define (get-edges loe)
  (foldr
   ; Exp -> ListOfSymbol
   ; RETURNS: the list of function names
   ; Strategy: case on whether given exp is var or app
   (lambda (e sofar)
     (cond [(appexp? e)
            (append (list (appexp-fn e))
                    (get-edges (appexp-args e))
                    sofar)]
           [(varexp? e) sofar]))
   empty
   loe))
;; TEST
(begin-for-test
  (check-equal?
   (get-edges
    (list (make-appexp 'f2
                       (list (make-appexp 'f3 empty)))))
   (list 'f2 'f3)
   "The function should return the first functions in the expression"))
;===============================================================================
;; get-all-fn-name : Program -> SetOfSymbol
;; GIVEN: a program
;; RETURNS: a set of all function names
;; Strategy: Use HOF foldr on program
(define (get-all-fn-name program)
  (map
   ;Definition -> Symbol
   ;RETURNS: the function name in the definition
   (lambda (p) (def-name p))
   program))

;; TEST
(begin-for-test
  (check-equal?
   (get-all-fn-name some-loops) (list 'f1 'f2 'f3 'f4 'f5 'no-loop)
   "This should return all the function names that are defined"))
;===============================================================================
;; TEST SAMPLE
(define some-loops
  (list
   (make-def 'f1 (list 'x) (make-appexp 'no-loop (list (make-varexp 'x))))
   (make-def 'f2 (list 'u 'y) (make-appexp 'f1 (list (make-varexp 'y))))
   (make-def 'f3 (list 'x 'u)
             (make-appexp 'f1 (list (make-appexp 'f4
                                                 (list (make-varexp 'u)
                                                       (make-varexp 'w)))
                                    (make-varexp 'z))))
   (make-def 'f4 (list 'x 'y)
             (make-appexp 'f5
                          (list (make-varexp 'y)
                                (make-varexp 'u))))
   (make-def 'f5 (list 'u)
             (make-appexp 'f2
                          (list (make-appexp 'f3 empty))))
   (make-def 'no-loop (list 'x) (make-varexp 'x))))

(define test-loops
  (list
   (make-def 'f1 (list 'x)
             (make-appexp 'no-loop
                          (list (make-appexp
                                 'f3
                                 (list (make-appexp
                                        'f4
                                        (list (make-varexp 'u)
                                              (make-varexp 'w))))))))
   (make-def 'f2 (list 'u 'y) (make-appexp 'f1 (list (make-varexp 'y))))))

;; list of nodes
(define NODES-TEST
  (list
   (make-node 'f1 (list 'no-loop))
   (make-node 'f2 (list 'f1))
   (make-node 'f3 (list 'f1 'f4))
   (make-node 'f4 (list 'f5))
   (make-node 'f5 (list 'f2 'f3))
   (make-node 'no-loop '())))


