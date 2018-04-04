;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(check-location "07" "q2.rkt")

(provide probe-possible-outcome?
         make-turn-right
         make-turn-left
         make-move-forward)

;; A Program is a ListOfInstruction
;; Interp: A sequence of instructions, to be executed from left to
;; right.
;; TEMPLATE:
#|
;; program-fn : Program -> ??
(define (program-fn p)
  (if (empty? p)...
      (... (ins-fn (first p))
           (program-fn (rest p))))
|#

(define-struct turn-left ())
(define-struct turn-right ())
(define-struct move-forward (step))
;; An Instruction is one of
;; -- (make-turn-left)            Interp: a turn-left instruction
;; -- (make-turn-right)           Interp: a turn-right instruction
;; -- (make-move-forward PosInt)  Interp: an instruction to move forward
;;                                        the given number of steps.
;; TEMPLATE:
#|
;; ins-fn : Instruction -> ??
(define (ins-fn i)
  (cond
    [(turn-left? i) ...]
    [(turn-right? i) ...]
    [(move-forward? i) (... (move-forward-step i))]))
|#

(define-struct dir (x y))
;; A DirectionVector is one of
;; -- (make-dir  0 -1) interp: facing north (up) 
;; -- (make-dir  0  1) interp: facing south (down)
;; -- (make-dir  1  0) interp: facing east (right)
;; -- (make-dir -1  0) interp: facing west (left)
;; TEMPLATE
#|
;; dir-fn : Direction -> ??
(define (dir-fn d)
  (cond
    [(equal? d (make-dir 0 -1)) (... (dir-x d) (dir-y d))]
    [(equal? d (make-dir -1 0)) (... (dir-x d) (dir-y d))]
    [(equal? d (make-dir 0 1)) (... (dir-x d) (dir-y d))]
    [(equal? d (make-dir 1 0)) (... (dir-x d) (dir-y d))]))
|#

(define-struct probe (x y dir))
;; A Probe is a (make-probe Integer Integer DirectionVector)
;; INTERPRETATION
;; x and y are Integers representing the position of the probe
;; dir is a Direction representing the direction the probe faces
;; TEMPLATE
#|
;; probe-fn : Probe -> ??
(define (probe-fn p)
  (... (probe-x p)
       (probe-y p)
       (dir-fn (probe-dir p))))
|#

(define-struct abs-probe (xv yv dir))
;; A AbstractProbe is a
;;   (make-abs-probe Range Range DirectionVector)
;; Interp:
;; -- xv and yv are all the possible x and y range values
;;                for the abstract probe respectively
;; -- dir is the direction of the probe
;; TEMPLATE
;; abs-probe-fn : AbstractProbe -> ??
#|
(define (abs-probe-fn ap)
  (... (range-fn (abs-probe-xv ap))
       (range-fn (abs-probe-yv ap))
       (dir-fn (abs-probe-dir ap))))
|#

(define-struct rg (high low))
;; A Range is a
;;     (make-rg Integer Integer)
;; Interpretation:
;; high and low are upper and lower bound of range
;; TEMPLATE
;; range-fn : Range -> ??
#|
(define (range-fn x)
     (... (range-high x)
          (range-low x)))
|#


;; a Velocity is one of
;; --  1
;; --  0 
;; -- -1
;; Interp: velocity made up the DirectionVector
;; TEMPLATE
;; velocity-fn : Velocity -> ??
#|
(define (velocity-fn v)
  (cond
    [(= v 0) ...]
    [(= v 1) ...]
    [(= v -1) ...]))
|# 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dir-turned-left : DirectionVector -> DirectionVector
;; RETURNS: the direction after turned left
;; NOTE:
;;  When turning left: North (0,-1) -> West (-1,0) -> South (0,1) -> East (1,0)
;;                       ^-------------------------------------------|
;;  and turning right is just turning left three times
;; Strategy: case on the direction
(define (dir-turned-left d)
  (cond
    [(equal? d (make-dir 0 -1)) (make-dir -1 0)]
    [(equal? d (make-dir -1 0)) (make-dir 0 1)]
    [(equal? d (make-dir 0 1)) (make-dir 1 0)]
    [(equal? d (make-dir 1 0)) (make-dir 0 -1)]))

;; TESTS
(begin-for-test
  (check-equal? (dir-turned-left (make-dir 1 0)) (make-dir 0 -1)
                "After a left turn facing east, the direction is north"))

;; abs-probe-turned-left: AbstractProbe -> AbstractProbe
;; abs-probe-turned-right: AbstractProbe -> AbstractProbe
;; GIVEN: an abstract probe
;; RETURNS: new abstract probe after the abstract probe turns left/right
;; Strategy: Use template for AbstractProbe on ap
(define (abs-probe-turned-left ap)
  (make-abs-probe (abs-probe-xv ap) (abs-probe-yv ap)
                  (dir-turned-left (abs-probe-dir ap))))

(define (abs-probe-turned-right ap)
  (make-abs-probe (abs-probe-xv ap) (abs-probe-yv ap)
                  (dir-turned-left
                   (dir-turned-left
                    (dir-turned-left (abs-probe-dir ap))))))

;; TESTS
(begin-for-test
  (check-equal?
   (abs-probe-turned-left TEST-ABS)
   (make-abs-probe (make-rg 20 20) (make-rg 100 100) (make-dir -1 0))
   "The probe should face west after left turn")
  (check-equal?
   (abs-probe-turned-right TEST-ABS)
   (make-abs-probe (make-rg 20 20) (make-rg 100 100) (make-dir 1 0))
   "The probe should face east after right turn"))

;; probe-possible-outcome? : Int Int Program Int Int -> Boolean
;; GIVEN: starting coordinates x0, y0, a robot program p, and ending
;; coordinates x1, y1.
;; RETURNS: true iff the robot, starting at (x0, y0) and facing north,
;; and executing program p according to the tolerances given above,
;; could end at (x1, y1).
;; EXAMPLES:
;; Let p1 = (list (make-turn-right)
;;                (make-move-forward 10)
;;                (make-turn-right)
;;                (make-move-forward 5))
;; then (probe-possible-outcome 20 100 p1 x1 y1) = true iff
;; x1 is in the interval [28, 32] and y1 is in the interval
;; [103,107].
;; Strategy: call a more general function
(define (probe-possible-outcome? x0 y0 program x1 y1)
  (in-range?
   x1 y1
   (probe-outcome
    (make-abs-probe (make-rg x0 x0) (make-rg y0 y0) (make-dir 0 -1))
    program)))

;; TESTS
(begin-for-test
  (check-equal? (probe-possible-outcome? 20 100 p1 29 105)
                #true
                "After following the program p1, x=29 and y=105 lie
                   within the range of possible values")
  (check-equal? (probe-possible-outcome? 20 100 p1 35 105)
                #false
                "After following the program p1, x=35 and y=105 do not lie
                  within the range of possible values"))

;; in-range? : Integer Integer AbstractProbe -> Boolean
;; RETURNS: true iff the given coordinates
;;          are with in range of the given list respectively
;; Strategy: combine simpler functions
(define (in-range? x y ap)
  (and
   (and (<= x (rg-high (abs-probe-xv ap)))
        (>= x (rg-low (abs-probe-xv ap))))
   (and (<= y (rg-high (abs-probe-yv ap)))
        (>= y (rg-low (abs-probe-yv ap)))
        )))

;; TESTS
(begin-for-test
  (check-equal?
   (in-range? 20 108 ABS-RANGE) #true
   "The values lie in the range of values specified by the abstract probe")
  (check-equal?
   (in-range? 20 105 ABS-RANGE) #false
   "The values do not lie in the range of values specified by the abstract probe"))

;; probe-outcome : AbstractProbe ListOfInstruction -> AbstractProbe
;; GIVEN: an abstract probe and a list of instructions loi
;; RETURNS: an abstract probe after some list of instruction
;; Strategy: use HOF foldl on loi
(define (probe-outcome ap loi)
  (foldl
   ;; Instruction AbstractProbe -> AbstractProbe
   ;; RETURNS: an abstract probe after some instruction i
   (lambda (i ap)
     (probe-after-instruction ap i))
   ap
   loi))

;; TEST
(begin-for-test
  (check-equal?
   (probe-outcome TEST-ABS p1)
   (make-abs-probe (make-rg 32 28) (make-rg 107 103) (make-dir 0 1))
   "The test abstract probe after following a program p1 faces south has
   x in the range (32,28) and y in the range (107,103)"))

;; probe-after-instruction : AbstractProbe Instruction -> ListOfProbe
;; GIVEN: an abstract probe p and a instruction i
;; RETURNS: the state of the abstract probe after the given instruction
;; Strategy: case on Instruction
(define (probe-after-instruction ap i)
  (cond
    [(turn-left? i) (abs-probe-turned-left ap)]
    [(turn-right? i) (abs-probe-turned-right ap)]
    [(move-forward? i) (probe-move-forward ap (move-forward-step i))]))

;; TESTS
(begin-for-test
  (check-equal?
   (probe-after-instruction TEST-ABS (make-turn-left))
   (make-abs-probe (make-rg 20 20) (make-rg 100 100) (make-dir -1 0))
   "After a left turn instruction, the abstract probe now faces west")
  (check-equal?
   (probe-after-instruction TEST-ABS (make-turn-right))
   (make-abs-probe (make-rg 20 20) (make-rg 100 100) (make-dir 1 0))
   "After a right turn instruction, the abstract probe now faces east")
  (check-equal?
   (probe-after-instruction TEST-ABS (make-move-forward 10))
   (make-abs-probe (make-rg 20 20) (make-rg 92 88) (make-dir 0 -1))
   "After a move forward instruction, the abstract probe has a range of (92,88)"))

;; probe-move-forward : AbstractProbe PosInt -> AbstractProbe
;; GIVEN: an abstract probe ap and a number of step
;; RETURN: all the possible instances of the probe after moving
;; Strategy: Use template for AbstractProbe on ap
(define (probe-move-forward ap step)
  (axis-move-forward (abs-probe-xv ap) (abs-probe-yv ap) step (abs-probe-dir ap)))

;; TESTS
(begin-for-test
  (check-equal?
   (probe-move-forward TEST-ABS 10)
   (make-abs-probe (make-rg 20 20) (make-rg 92 88) (make-dir 0 -1))
   "After moving 10 steps north, the possible values of y lie in the
   range (92,88)"))

;; axis-move-forward : PossibleX PossibleY PosInt DirectionVector
;;                     -> AbstractProbe
;; GIVEN: the range of x and y values of the probe,
;;        the number of steps, a direction
;; RETURNS: the new abstract probe
;; Strategy: combine simpler functions
(define (axis-move-forward x y step d)
  (make-abs-probe
  (range-union 
    (axis-forward (rg-high x) step (dir-x d))
    (axis-forward (rg-low x) step (dir-x d)))
  (range-union 
    (axis-forward (rg-high y) step (dir-y d))
    (axis-forward (rg-low y) step (dir-y d)))
   d))

;; TESTS
(begin-for-test
  (check-equal?
   (axis-move-forward
    (make-rg 25 22)
    (make-rg 100 90)
    10
    (make-dir 0 1))
   (make-abs-probe (make-rg 25 22) (make-rg 112 98) (make-dir 0 1))
   "The new range of values for the abstract probe should be from 112 to 98"))


;; axis-forward : Integer PosInt Velocity -> Range
;; GIVEN: the position, number of steps
;;        and the direction vector along the x/y axis
;; RETURNS: the new possible low value for the position of the probe
;; Startegy: combine simpler functions
(define (axis-forward val step d)
  (if (= d 0) (make-range val val)
      (make-range
       (two-more-step (right-val val step d) d)
       (two-less-step (right-val val step d) step d))))
  
;; TESTS
(begin-for-test
  (check-equal? (axis-forward 20 10 1) (make-rg 32 28)
                "The possible low value for 20 with step 10 is 20")
  (check-equal? (axis-forward 20 1 -1) (make-rg 20 17)
                "The possible low value for step 1 will be 20"))


;; two-less-step : Int PosInt Velocity -> Int
;; GIVEN: the probe position, step and velocity 
;; RETURNS: the new probe position
;; Strategy: combine simpler functions
(define (right-val val step d)
  (+ val (* step d)))

;; TEST
(begin-for-test
  (check-equal? (right-val 20 2 1) 22
                "The new position will be 22"))


;; two-more-step : Int Velocity -> Int
;; GIVEN: the probe position and velocity
;; RETURNS: adds the additional tolerance to the position
;; Strategy: combine simpler functions
(define (two-more-step r-val d)
      (+ r-val (* d 2)))

;; TEST
(begin-for-test
  (check-equal? (two-more-step 22 -1) 20
                "The new position will be 20"))

;; two-less-step : Int NonNegInt Velocity -> Int
;; GIVEN: the actual probe co-ordinate value, step and velocity
;; RETURNS: new co-ordinate value depending on step
;; Strategy: combine simpler functions
(define (two-less-step r-val step d)
  (cond
    [(= step 1) (- r-val d)]
    [else (- r-val (* d 2))]))
;; TEST
(begin-for-test
  (check-equal? (two-less-step 20 1 -1)
                21
                "should stay still when move is less than 2"))


;; range-union : Range Range -> Range
;; GIVEN: Two range of values
;; RETURNS: a range with the maximum and minimum values from the two ranges
;; Strategy: Use template of Range on r1,r2
(define (range-union r1 r2)
  (make-range
   (max (rg-high r1) (rg-low r1) (rg-high r2) (rg-low r2))
   (min (rg-high r1) (rg-low r1) (rg-high r2) (rg-low r2))))

;; TEST
(begin-for-test
  (check-equal? (range-union (make-rg 30 17) (make-rg 22 15)) (make-rg 30 15)
                "The range should now lie between 30 and 15"))

;; swap-high-low : Range -> Range
;; GIVEN: a range
;; REUTRNS: the high and low values swaped if high is lower than low
;; Strategy: use template for Range on r
(define (swap-high-low r)
  (if (< (rg-high r) (rg-low r))
      (make-rg (rg-low r) (rg-high r))
      r))
;; TEST
(begin-for-test
  (check-equal? (swap-high-low (make-rg 10 20))
                (make-rg 20 10)
                "if illegal, values should be swaped")
  (check-equal? (swap-high-low (make-rg 30 20))
                (make-rg 30 20)
                "if legal, values should remain the same"))

;; make-range: Integer Integer -> Range
;; GIVEN: two integers
;; RETURNS: a legal range from two the two values,
;;          makeing the bigger interger the "high" value
;; Strategy: Use definition for Range
(define (make-range x y)
  (swap-high-low (make-rg x y)))
;; TEST
(begin-for-test
  (check-equal? (make-range 30 20)
                (make-rg 30 20)
                "if value order is correct, order remain still")
  (check-equal? (make-range 10 20)
                (make-rg 20 10)
                "if value order is wrong, order swaped"))

;; TEST CONSTANT

(define p1 (list (make-turn-right)
                 (make-move-forward 10)
                 (make-turn-right)
                 (make-move-forward 5)))

(define TEST-ABS
  (make-abs-probe (make-rg 20 20) (make-rg 100 100) (make-dir 0 -1)))

(define ABS-RANGE
  (make-abs-probe (make-rg 24 14) (make-rg 113 107) (make-dir 0 -1)))






