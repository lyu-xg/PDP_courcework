;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "this")
(provide string-last)

;; string-last : String -> 1String
;; RETURNS: the last character of the given string (cluding "\\" "\t" etc.)
;; Example:
;; (string-last "string") => "g"
;; (string-last "hello world!") => "!"
;; Design Strategy: Combine simpler functions
(define (string-last str) (string-ith str (- (string-length str) 1)))
;; Test cases:
(begin-for-test
  (check-equal? (string-last "something cool") "l" "string-last function does not work properly")
  (check-equal? (string-last "hello") "o" "string-last function does not work properly")
)