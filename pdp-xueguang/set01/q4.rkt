;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exsercise19) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "this")
(provide string-insert)

;; string-insert : string number -> string
;; GIVEN: a string and a number
;;        WHERE i is a integer belongs to [0, (string-length str)]
;; RETURNS: a string with ith character being "_" and all other characters thereafter becomes (n+1)th character
;; Examples:
;; (string-insert "hello" 3) => "hel_lo"
;; (string-insert "world" 2) => "wo_rld"
(define (string-insert str i) (string-append (substring str 0 i) "_" (substring str i (string-length str))))
(begin-for-test
  (check-equal? (string-insert "something" 4) "some_thing" "string-insert does not work as designed")
  (check-equal? (string-insert "helloworld" 5) "hello_world" "string-insert does not work as designed")
  )
;; Addtional Question: Ponder how string-insert copes with "".
;; Answer: when str is empty, the possible i values belongs to [0, 0], which is 0
;;         therefore, empty string case works and only works like this: (string-insert "" 0) => "_"