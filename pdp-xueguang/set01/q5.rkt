;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise20) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "this")
(provide string-delete)

;; string-delete : string number -> string
;; GIVEN: a non-empty string (str) and a number (i)
;;        WHERE i is a integer belongs to [0, (string-length str))
;; RETURNS: a string with ith character deleted
;; Examples:
;; (string-delete "bhello" 0) => "hello"
;; (string-delete "world" 4) => "worl"
;; Design strategy: Combine simpler functions
(define (string-delete str i) (string-append (substring str 0 i) (substring str (+ i 1) (string-length str))))
(begin-for-test
  (check-equal? (string-delete "something" 5) "someting" "string-delete does not work as designed")
  (check-equal? (string-delete "hello" 4) "hell" "string-delete does not work as designed")
  )
;; Addtional question: Can string-delete deal with empty strings?
;; Answer: No, since given number should be positive but smaller or equal to the length of the string,
;; when the length of the string is 0, no real number matches such requirement;
;; therefore, empty string is not accepted.