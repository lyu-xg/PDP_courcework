;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

(check-location "08" "q2.rkt")

(provide make-pos
         make-neg
         make-clause
         is-null-derivable?)
;===============================================================================
;; A Variable is a Racket Symbol.
;===============================================================================
;; A ListOfX (LOX) is a one of
;; -- empty
;; -- (cons X LOX)
;; TEMPLATE:
;; lox-fn : lox -> ??
#;
(define (lox-fn lox)
  (if (empty? lox) ...
      (... (x-fn (first lox))
           (lox-fn (rest lox)))))
;===============================================================================
;; A SetOfX (EOX) is a LOX without duplications
;; NOTE: empty is a SetOfX
;; TEMPLATE:
;; lox-fn : lox -> ??
#;
(define (lox-fn lox)
  (if (empty? lox) ...
      (... (x-fn (first lox))
           (lox-fn (rest lox)))))
;===============================================================================
(define-struct pos (var))
;; A PositiveLiteral is (make-pos Variable)
;; Interp: a literal containing the variable
;; pos-fn : PositiveLiteral -> ??
#;
(define (pos-fn p)
  (... (pos-var p)))
;===============================================================================
(define-struct neg (var))
;; A NegativeLiteral is (make-neg Variable)
;; Interp: a literal containing the negation of the variable
;; neg-fn : NegativeLiteral -> ??
#;
(define (neg-fn n)
  (... (neg-var n)))
;===============================================================================
;; A Literal is one of
;; -- PositiveLiteral
;; -- NegativeLiteral
;; TEMPLATE
;; literal-fn : Leteral -> ??
#;
(define (literal-fn l)
  (cond
    [(pos? l) (... (pos-fn l))]
    [(neg? l) (... (neg-fn l))]))
;===============================================================================
(define-struct my-clause (pos-set neg-set))
;; A Clause is a (make-my-clause SetOfVariable SetOfVariable)
;; INTERP:
;; -- pos-set is a set of variable who are positive
;; -- neg-set is a set of variable who are negative
;; WHERE: pos-set and neg-set are disjoint sets
;; TEMPLATE:
;; my-clause-fn : my-clause -> ??
#;
(define (my-clause-fn myc)
  (... (sov-fn (my-clause-pos-set myc))
       (sov-fn (my-clause-neg-set myc))))
;; Alias
(define clause-pos my-clause-pos-set)
(define clause-neg my-clause-neg-set)
;===============================================================================
;; A MayBeX is one of
;; -- false
;; -- X
;; TEMPLATE:
;; maybex-fn : MayBeX -> ??
#;
(define (maybex-fn x)
  (cond [(x? x) (... (x-fn x))]
        [else ...]))
;===============================================================================
;; END OF DATA DEFINITION

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTIONS
;===============================================================================
;; make-clause : ListOfLiteral -> Clause
;; GIVEN: a list of literals, possibly with duplications
;; RETURNS: a clause containing exactly those literals
;; Strategy: use data definition of Clause
(define (make-clause lol)
  (make-my-clause
   (list-to-set (make-list-of-pos-var lol))
   (list-to-set (make-list-of-neg-var lol))))
;; TEST
(begin-for-test
  (check-true (set-equal? (my-clause-pos-set (make-clause LAB))
                          (list 'a 'b))
              "a b should be added to positive set of the clause structure"))
;===============================================================================
;; combine-all-clause : ListOfClause -> Clause
;; RETURNS: a clause with all the given clause combined into one
;; Strategy: use HOF foldr 
(define (combine-all-clause loc)
  (foldr combine-clause EMPTY-CLAUSE loc))
;; TEST
(begin-for-test
  (check-equal?
   (combine-all-clause
    (list (make-my-clause (list 1 3) (list 9 8))
          (make-my-clause (list 3 5) (list 7 0))))
   (make-my-clause (list 1 3 5) (list 9 8 7 0))
   "combine-all-clause should union all pos-set and neg-set"))
;===============================================================================
;; eliminating-singletons : SetOfClause -> SetOfClause
;; RETURNS: the set of claus with all the singletons removed
;; Strategy: combine simpler functions
(define (eliminate-singletons soc)
  (eliminate-clause-with soc (singleton-vars soc)))
;; TEST
(begin-for-test
  (check-equal? (eliminate-singletons LOC-EXAMPLE)
                (list A~BC ~AC CLAUSE-B CLAUSE-~C)
                "the D should be removed because it contain singleton"))
;===============================================================================
;; singleton-vars : SetOfClause -> Clause
;; GIVEN: a set of clause
;; RETURNS: a clause that represents all singletons vars
;; Strategy: use template for Clause on combined
(define (singleton-vars soc)
  (let [(combined (combine-all-clause soc))]
    (make-my-clause
     (set-diff (clause-pos combined) (clause-neg combined))
     (set-diff (clause-neg combined) (clause-pos combined)))))
;; TEST
(begin-for-test
  (check-equal? (singleton-vars LOC-EXAMPLE)
                (make-my-clause (list 'd) empty)
                "positive d is the only singleton in example"))
;===============================================================================
;; eliminate-clause-with : SetOfClause Clause -> SetOfClause
;; GIVEN: a set of clause soc
;;        a clause vars representing the vars that should be removed
;; RETURNS: a set of clause with clauses that contains vars removed
;; Strategy: use HOF filter on soc
(define (eliminate-clause-with soc vars)
  (filter
   ;; Clause -> Boolean
   ;; RETURNS: true iff given clause contains given vars
   (lambda (c) (singleton? c vars))
   soc))
;; TEST
(begin-for-test
  (check-equal?
   (eliminate-clause-with LOC-EXAMPLE
                          (make-my-clause (list 'd) empty))
   (list A~BC ~AC CLAUSE-B CLAUSE-~C)
   "eliminate-clause-with should remove D from EXAMPLE"))
;===============================================================================
;; singleton? : Clause Clause -> Boolean
;; GIVEN: a clause c, and a clause representing singleton vars
;; RETURNS: true iff the given clause doesn't contains singleton vars
;; Strategy: use template for Clause on c and singletons
(define (singleton? c singletons)
  (and (empty? (set-intersect (clause-pos singletons)
                             (clause-pos c)))
       (empty? (set-intersect (clause-neg singletons)
                             (clause-neg c)))))
;; TEST
(begin-for-test
  (check-false (singleton? (make-my-clause (list 'd 'b) '())
                           (make-my-clause (list 'd) empty))
               "DB should be a singleton clause because of D")
  (check-true  (singleton? (make-my-clause empty (list 'c))
                           (make-my-clause (list 'd) empty))
               "~C is not a singleton clause because of no D"))
;===============================================================================
;; is-null-derivable? : ListOfClause -> Boolean
;; GIVEN: a list of clauses
;; RETURNS: true iff the empty clause is derivable from the given
;;          clauses using the rule of resolution
;; Strategy: use HOF ormap on loc
(define (is-null-derivable? loc)
  (let [(workvars (clause-common (combine-all-clause loc)))]
    (null-derivable? (list-to-set loc) workvars)))
;; TEST
(begin-for-test
  (check-true (is-null-derivable? LOC-EXAMPLE)
              "the given example should be true")
  (check-false
   (is-null-derivable? LOC-EXAMPLE-WITHOUT-C)
   "the given example without c should not be able to derive empty"))
;===============================================================================
;; null-derivable? : SetOfClause SetOfVariable -> Boolean
;; GIVEN: a set of clause, a set of variables
;; WHERE: workvars contains no more vars that is in workset
;; RETURNS: true iff the empty clause is derivable from the given
;;          clauses using the rule of resolution
;; Strategy: Davis-Putnam procedure
;;           if empty clause is found in the workset, return true (yah!)
;;           else if there is no more clause in the work set, return false
;;           else recur under the assumption that the first var in the workvars
;;                is true/false with updated workset and (rest workvars)
;; Haulting Measure: 2^(length workvars)
(define (null-derivable? soc workvars)
  (let [(workset (eliminate-singletons soc))]
    (cond
      [(member? EMPTY-CLAUSE workset) true]
      [(empty? workset) false]
      [else
       (and (null-derivable? (eliminate-var-when-false workset (first workvars))
                            (rest workvars))
           (null-derivable? (eliminate-var-when-true workset (first workvars))
                            (rest workvars)))])))
;; TEST
(begin-for-test
  (check-true (null-derivable?
               LOC-EXAMPLE
               (clause-common (combine-all-clause LOC-EXAMPLE)))
              "the given EXAMPLE should be able to derive an empty clause"))
;===============================================================================
;; eliminate-var-when-true : SetOfClause Variable -> SetOfClause
;; GIVEN: a set of clause c, a variable v
;; RETURNS: the set of clause updated under the assumption that v is true
;; Strategy: use HOF foldr on soc
(define (eliminate-var-when-true soc v)
  (foldr
   ;; Clause SetOfClause -> SetOfClause
   ;; RETURNS: the set of clause with the updated clause
   (lambda (c sofar)
     (cond
       [(member? v (clause-pos c)) sofar]
       [(member? v (clause-neg c)) (cons (eliminate-var-in-neg c v) sofar)]
       [else (cons c sofar)]))
   empty
   soc))
;; TEST
(begin-for-test
  (check-equal?
   (eliminate-var-when-true LOC-EXAMPLE 'a)
   (list
    (make-my-clause (list 'd 'b) empty)
    (make-my-clause (list 'c) empty)
    (make-my-clause (list 'b) empty)
    (make-my-clause empty (list 'c)))
   "eliminating 'a should remove clause when 'a is in pos-set
                 and remove 'a from clause when 'a in neg-set"))
;===============================================================================
;; eliminate-var-when-true : SetOfClause Variable -> SetOfClause
;; GIVEN: a set of clause c, a variable v
;; RETURNS: the set of clause updated under the assumption that v is false
;; Strategy: use HOF foldr on soc
(define (eliminate-var-when-false soc v)
  (foldr
   ;; Clause SetOfClause -> SetOfClause
   ;; RETURNS: the set of clause with the updated clause
   (lambda (c sofar)
     (cond
       [(member? v (clause-pos c)) (cons (eliminate-var-in-pos c v) sofar)]
       [(member? v (clause-neg c)) sofar]
       [else (cons c sofar)]))
   empty
   soc))
;; TEST
(begin-for-test
  (check-equal?
   (eliminate-var-when-false LOC-EXAMPLE 'a)
   (list
    (make-my-clause (list 'c) (list 'b))
    (make-my-clause (list 'd 'b) empty)
    (make-my-clause (list 'b) empty)
    (make-my-clause empty (list 'c)))
   "eliminating 'a when false should remove clause when 'a is in pos-set
    and remove 'a from clause when 'a in neg-set"))
;===============================================================================
;; eliminate-var-in-pos : Clause Variable -> Clause
;; GIVEN: a clause and a variable
;; RETURNS: the clause with the variable removed from clause's pos-set
;; Strategy: use template for Clause on c
(define (eliminate-var-in-pos c v)
  (make-my-clause (set-diff (clause-pos c) (list v)) (clause-neg c)))
;; TEST
(begin-for-test
  (check-equal?
   (eliminate-var-in-pos (make-my-clause (list 1 2 3) (list 1 2 3)) 2)
   (make-my-clause (list 1 3) (list 1 2 3))
   "the 2 should be removed from only pos-set"))
;===============================================================================
;; eliminate-var-in-neg : Clause Variable -> Clause
;; GIVEN: a clause and a variable
;; RETURNS: the clause with the variable removed from clause's pos-set
;; Strategy: use template for Clause on c
(define (eliminate-var-in-neg c v)
  (make-my-clause (clause-pos c) (set-diff (clause-neg c) (list v)) ))
;; TEST
(begin-for-test
  (check-equal?
   (eliminate-var-in-neg (make-my-clause (list 1 2 3) (list 1 2 3)) 2)
   (make-my-clause (list 1 2 3) (list 1 3))
   "the 2 should be removed from only neg-set"))
;===============================================================================
;; END OF CORE FUNCTIONS


;; HELPER FUNCTIONS
;===============================================================================
;; combine-clause : Clause Clause -> Clause
;; GIVEN: two clauses
;; RETURNS: the combination of the two clause
;;          simply adding them together
;; Strategy: use template for Clause on c1 and c2
(define (combine-clause c1 c2)
  (make-my-clause
   (set-union (clause-pos c1) (clause-pos c2))
   (set-union (clause-neg c1) (clause-neg c2))))
;; TEST
(begin-for-test
  (check-equal? (combine-clause AB~E AB~C~D)
                AB~C~D~E
                "combine clause should simply combine pos-set and neg-set"))
;===============================================================================
;; list-to-set : ListOfX -> SetOfX
;; RETURNS: the list without duplications
;; Strategy: use HOF foldl on lox
(define (list-to-set lox)
  (foldl
   ;; X SetOfX -> SetOfX
   ;; RETURNS: the set of x after added given x
   (lambda (x sofar) (set-cons x sofar))
   empty
   lox))
;; TEST
(begin-for-test
  (check-equal? (length (list-to-set (list 1 1 2 2)))
                2 "list-to-set should reduce this list size to half"))
;===============================================================================
;; make-list-of-pos-var : ListOfLiteral -> SetOfVariable
;; GIVEN: a list of literals, possibly with duplications
;; RETURNS: a set of variable that are positive Literals
;; Strategy: use HOF filter & map on lol
(define (make-list-of-pos-var lol)
  (map
   ;; Literal -> Variable
   ;; RETURNS: the variable name of given literal
   (lambda (l) (pos-var l))
   (filter pos? lol)))
;; TEST
(begin-for-test
  (check-equal? (make-list-of-pos-var LAB~C~D)
                (list 'a 'b)
                "make-list-of-pos-var should disregard negative C and D"))
;===============================================================================
;; make-list-of-neg-var : ListOfLiteral -> SetOfVariable
;; GIVEN: a list of literals, possibly with duplications
;; RETURNS: a set of variable that are positive Literals
;; Strategy: use HOF filter & map on lol
(define (make-list-of-neg-var lol)
  (map
   ;; Literal -> Variable
   ;; RETURNS: the variable name of given literal
   (lambda (l) (neg-var l))
   (filter neg? lol)))
;; TEST
(begin-for-test
  (check-equal? (make-list-of-neg-var LAB~C~D)
                (list 'c 'd)
                "make-list-of-neg-var should disregard positive A and B"))
;===============================================================================
;; set-intersect : SetOfX SetOfX -> SetOfX
;; GIVEN: two sets of X
;; RETURNS: the intersection of the given two sets
;; Strategy: use HOF filter on sox1
(define (set-intersect sox1 sox2)
  (let ([diff (set-diff sox1 sox2)])
    (if (equal? sox1 diff) empty
        (filter
         ;; X -> Boolean
         ;; RETURNS: true iff given X is in diff
         (lambda (x) (not (member? x diff)))
         sox1))))
;; TEST
(begin-for-test
  (check-equal? (set-intersect (list 1 2 3) (list 2 3 4))
                (list 2 3)
                "the intersect of 123 and 234 should be 23")
  (check-equal? (set-intersect (list 1 2 3) (list 4 5 6))
                empty
                "the intersect of 123 and 456 should be empty"))
;===============================================================================
;; clause-common : Clause -> SetOfVariable
;; GIVEN: a clause
;; RETURNS: common variables for pos-set and neg-set of the given clause
;; Strategy: use template for Clause on c
(define (clause-common c)
  (set-intersect (clause-pos c) (clause-neg c)))
;; TEST
(begin-for-test
  (check-equal?
   (clause-common (make-my-clause (list 'a 'b 'c) (list 'c 'b 'd)))
   (list 'b 'c)
   "the common variable of abc and bcd should be b and c"))
;===============================================================================
;; clause-empty? : Clause -> Boolean
;; GIVEN: a clause
;; RETURNS: true iff the clause is empty
;; Strategy: use template for Clause on c
(define (clause-empty? c)
  (and (empty? (clause-pos c))
       (empty? (clause-neg c))))
;; TEST
(begin-for-test
  (check-true (clause-empty? (make-my-clause empty empty))
              "when both lists are empty, the clause should be empty"))
;===============================================================================

;; TEST CONSTANTS

;; Literals
(define  A (make-pos 'a))
(define ~A (make-neg 'a))
(define  B (make-pos 'b))
(define ~B (make-neg 'b))
(define  C (make-pos 'c))
(define ~C (make-neg 'c))
(define  D (make-pos 'd))
(define ~D (make-neg 'd))
(define  E (make-pos 'e))
(define ~E (make-neg 'e))

;; ListOfLiterals
(define LAB (list A B A))
(define LAB~C~D (list A B ~C ~D))

;; Clauses
(define EMPTY-CLAUSE (make-my-clause empty empty))
(define AB (make-clause LAB))
(define AB~C~D (make-clause LAB~C~D))
(define BD (make-clause (list B D)))
(define AB~E (make-clause (list A B ~E)))
(define AB~C~D~E (make-clause (list A B ~C ~D ~E)))
(define A~BC (make-clause (list A ~B C)))
(define AC~E (make-clause (list A C ~E)))
(define AC~E~C (make-clause (list A C ~E ~C)))
(define ~AC (make-clause (list ~A C)))
(define CLAUSE-B (make-clause (list B)))
(define CLAUSE-~C (make-clause (list ~C)))
(define CLAUSE-~B (make-clause (list ~B)))
(define A~C (make-clause (list A ~C)))
(define ~AD (make-clause (list ~A D)))
(define C~E (make-clause (list C ~E)))

;; ListOfClause
(define LOC-EXAMPLE           (list A~BC BD ~AC CLAUSE-B CLAUSE-~C))
(define LOC-EXAMPLE-WITHOUT-C (list A~BC BD ~AC CLAUSE-B))
(define TEST1 (list A~C ~AD C~E))


;; ADDITIONAL TESTS
(begin-for-test
  (check-true  (is-null-derivable? LOC-EXAMPLE))
  (check-false (is-null-derivable? LOC-EXAMPLE-WITHOUT-C))
  (check-false (is-null-derivable? TEST1)))




