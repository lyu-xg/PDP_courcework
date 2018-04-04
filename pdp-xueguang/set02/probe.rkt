;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require rackunit)
(require "extras.rkt")
(check-location "02" "probe.rkt")

(provide probe-at
         probe-turned-left
         probe-turned-right
         probe-direction-equal?
         probe-location-equal?
         probe-forward-possible-outcome?
         )

;; ==============================================================================

;; FacingDirection is one of
;; -- (list 0 -1) interp: facing north (up) 
;; -- (list 0 1)  interp: facing south (down)
;; -- (list 1 0)  interp: facing east (right)
;; -- (list -1 0) interp: facing west (left)
;;
;; Observer Template:
;; direction-fn : FacingDirection -> ??
;; (define (direction-fn fd)
;;   ...
;;   (frist fd) ; being the x value of FacingDirection
;;   (second fd) ; being the y value of FacingDirection
;; )

;; ==============================================================================

;; a Location is
;; -- (list X Y)
;; WHERE X and Y are Integers
;; Interp: X and Y are coordinates of the Location
;; Example: (list 0 0)
;;
;; Observer Template:
;; location-fn : Location -> ??
;; (define (location-fn pstn)
;;   ...
;;   (frist pstn) ; being the X coordinate of Location
;;   (second pstn) ; being the Y coordinate of Location
;; )

;; ==============================================================================

(define-struct probe (location direction))
;; A Probe is:
;; -- (make-probe Postion FacingDirection)
;; Examples:
;; (make-probe (list 0 0) (list -1 0)) ;; a Probe at (0,0) facing west
;; (make-probe (list 2 5) (list 0 -1)) ;; a Probe at (2,5) facing north
;;
;; Oberser Template:
;; probe-fn : Probe -> ??
;; (define (probe-fn p) ...
;;   (postion-fn (probe-location p))
;;   (direction-fn (probe-direction p))
;; )

;; ==============================================================================

;; probe-at: Integer Integer -> Probe
;; GIVEN: an x-coordinate and a y-coordinate
;; RETURNS: a probe with its center at those coordinates, facing north.
(define (probe-at x y) (make-probe (list x y) (list 0 -1)))
;; examples:
;; (probe-at 0 0) => (make-probe (list 0 0) (list 0 -1))
;; (probe-at 20 5) => (make-probe (list 20 5) list(0 -1))

;; ==============================================================================

;; probe-turned-left : Probe -> Probe
;; probe-turned-right : Probe -> Probe
;; GIVEN: a probe
;; RETURNS: a probe like the original, but turned 90 degrees either left
;;          or right.
;; Rationale:
;;  When turning left: North (0,-1) -> West (-1,0) -> South (0,1) -> East (1,0)
;;                       ^-------------------------------------------|
;;  and turning right is just the reverse of turning left
;; Design Strategy: Probe's template + case on FacingPostion
(define (probe-turned-left p)
  (cond
    [(equal? (probe-direction p) (list 0 -1))
      (make-probe (probe-location p) (list -1 0))]
    [(equal? (probe-direction p) (list -1 0))
      (make-probe (probe-location p) (list 0 1))]
    [(equal? (probe-direction p) (list 0 1))
      (make-probe (probe-location p) (list 1 0))]
    [(equal? (probe-direction p) (list 1 0))
      (make-probe (probe-location p) (list 0 -1))]
    ))

(define (probe-turned-right p)
  (cond
    [(equal? (probe-direction p) (list 0 -1))
      (make-probe (probe-location p) (list 1 0))]
    [(equal? (probe-direction p) (list 1 0))
      (make-probe (probe-location p) (list 0 1))]
    [(equal? (probe-direction p) (list 0 1))
      (make-probe (probe-location p) (list -1 0))]
    [(equal? (probe-direction p) (list -1 0))
      (make-probe (probe-location p) (list 0 -1))]
    ))

;; example:
;;(probe-turned-left (probe-at 0 0)) => (make-probe (list 0 0) (list -1 0))

;; ==============================================================================

;; probe-direction-equal? : Probe Probe -> Boolean
;; GIVEN: two probes
;; RETURNS: true iff the two probes are facing in the same direction
;; Design Strategy: use template of Probe
(define (probe-direction-equal? q1 q2)
  (equal? (probe-direction q1) (probe-direction q2)))
;; example:
;; (probe-direction-equal? (probe-at 1 1) (probe-at 0 0) => true
;; (probe-direction-equal? (make-probe (list 0 0) (list -1 0)) (probe-at 0 0))
;; => false

;; ==============================================================================

;; probe-location-equal? : Probe Probe -> Boolean
;; GIVEN: two probles
;; RETURNS: true iff the two probes are at the same location
;; Design Strategy: use template of Probe
(define (probe-location-equal? q1 q2)
  (equal? (probe-location q1) (probe-location q2)))
;; example:
;; (probe-location-equal? (probe-at 1 1) (make-probe (list 1 1) (list -1 0))
;; => true

;; ==============================================================================

;; probe-forward-possible-outcome? : Probe PosInt Probe -> Boolean
;; GIVEN: two probes and a distance
;; RETURNS: true iff the first probe, given a move-forward command with
;;          the specified number of steps, could wind up in the state 
;;          described by the second probe.

(define (probe-forward-possible-outcome? p1 step p2)
  (cond [(equal? (dy p1 p2) 0)
         (and (equal? (second (probe-direction p1)) 0) 
              (cond[(> (dx p1 p2) 0) (equal? (first (probe-direction p1)) 1)]
                   [(< (dx p1 p2) 0) (equal? (first (probe-direction p1)) -1)]))]
        [(equal? (dx p1 p2) 0)
         (and (equal? (first (probe-direction p1)) 0)
              (cond[(> (dy p1 p2) 0) (equal? (second (probe-direction p1)) 1)]
                   [(< (dy p1 p2) 0) (equal? (second (probe-direction p1)) -1)]))]
        [(equal? step 0) (probe-location-equal? p1 p2)]
        [else false])
)
;; example:
;; (probe-forward-possible-outcome? (probe-turned-right (probe-at 1 1)) 1
;;                                  (make-probe (probe-at 3 1))
;; => true

;; ==============================================================================

;; dx (dy) : Probe Probe -> Int
;; GIVEN: two Probes
;; RETURNS: the delta x (y) value of the locations of the two Probes
(define (dx p1 p2) (- (first (probe-location p2)) (first (probe-location p1))))
(define (dy p1 p2) (- (second (probe-location p2)) (second (probe-location p1))))
;; example:
;; (dx (probe-at 1 1) (probe-at 5 2)) => 4
;; (dy (probe-at 1 1) (probe-at 2 1)) => 0

(begin-for-test

  (check-equal? (probe-turned-left (probe-at 0 0))
                (make-probe (list 0 0) (list -1 0)))
  (check-equal? (probe-turned-left (probe-turned-left (probe-at 1 1)))
                (probe-turned-right (probe-turned-right (probe-at 1 1))))
  (check-equal? (probe-turned-left (probe-turned-left
                                    (probe-turned-left (probe-turned-left (probe-at 0 0)))))
                (probe-at 0 0))
  (check-equal? (probe-turned-right (probe-turned-right
                                    (probe-turned-right (probe-turned-right (probe-at 0 0)))))
                (probe-at 0 0))
  (check-equal? (probe-direction-equal? (probe-at 1 1) (probe-at 0 0))
                true)
  (check-equal? (probe-direction-equal? (make-probe (list 0 0) (list -1 0))
                                        (probe-at 0 0))
                false)
  (check-equal? (probe-location-equal? (probe-at 1 1)
                                       (make-probe (list 1 1) (list -1 0)))
                true)
  (check-equal? (probe-forward-possible-outcome? (probe-at 1 1) 1
                                                 (make-probe (list 1 1)
                                                             (list -1 0)))
                false)


  (check-equal? (probe-forward-possible-outcome? (probe-turned-right (probe-at 1 1)) 1
                                                 (probe-at 3 1))
                true)
  (check-equal? (probe-forward-possible-outcome? (probe-turned-left (probe-at 1 1)) 1
                                                 (probe-at 3 1))
                false)
  (check-equal? (probe-forward-possible-outcome? (probe-turned-left (probe-at 1 1)) 1
                                                 (probe-at -3 1))
                true)
  (check-equal? (probe-forward-possible-outcome? (probe-at 1 1) 100
                                                 (probe-at 1 -1))
                true)
  (check-equal? (probe-forward-possible-outcome? (make-probe (list 0 0) (list 0 1)) 0
                                                 (probe-at 1 10))
                false)
  (check-equal? (probe-forward-possible-outcome? (make-probe (list 0 0) (list 0 1)) 1
                                                 (probe-at 0 10))
                true)

  (check-equal? (probe-forward-possible-outcome? (make-probe (list 1 0) (list 0 1)) 1
                                                 (probe-at 0 10))
                false)

 )