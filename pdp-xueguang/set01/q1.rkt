;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "this")
(provide distance-to-origin)

;; distance-to-origin : x y -> d
;; GIVEN: x and y as two coordinates of a 2 dimensional point
;; RETURNS: d as the distance between the given point and origin
;; Example: 
;; (distance-to-origin 3 4) => 5
;; Design Strategy: Combine simpler functions
(define (distance-to-origin x y) (sqrt (+ (* x x) (* y y))))
;; Test cases:
(begin-for-test
  (check-eq? (distance-to-origin 3 4) 5 "distance-to-origin function does not as designed")
  (check-eq? (distance-to-origin 0 10) 10 "distance-to-origin function does not work as designed")
)