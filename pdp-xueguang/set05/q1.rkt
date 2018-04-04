;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)

(provide felleisen-roster
         shivers-roster
         possible-roster?
         acceptable-felleisen-answer?
         make-slip
         slip-color
         slip-name1
         slip-name2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct slip (color name1 name2))
;; A Slip is a (make-slip Color String String)
;; template:
;; (define (slip-fn s)
;;   (... (slip-color s) (slip-name1 s) (slip-name2)...))

;; A Color is one of
;; -- "yellow"
;; -- "blue"

;; A ListOfSlip is one of
;; -- empty
;; (cons Slip ListOfSlip)
;; template:
;; (define (los-fn loc)
;;   (if (empty? los) (...)
;;       (... (slip-fn (first los))
;;            (los-fn  (rest los))...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTIONS

;; color-filter : ListOfSlip Color -> ListOfSlip
;; GIVEN: a list of Slips
;; RETURNS: a list of Slips containing all the Slips with the given number
;; Strategy: use HOF filter on los
(define (color-filter los c)
  (filter
   ;; Slip -> Boolean
   ;; RETURNS: true iff the given Slip's color is equal to the global Color c
   (lambda (s) (string=? (slip-color s) c))
   los))
;; TEST
(begin-for-test
    (check-equal? (color-filter LIST1 "yellow") LIST1-YELLOW
                "color filter should filter out the blue slips"))


;; ordered-name=? : Slip Slip -> Boolean
;; GIVEN: two Slips
;; RETURNS: true iff the two Slips have the same name1 and name2
;; Strategy: use template of Slip
(define (ordered-name=? s1 s2)
  (and (string=? (slip-name1 s1) (slip-name1 s2))
       (string=? (slip-name2 s1) (slip-name2 s2))))
;; TEST
(begin-for-test
  (check-true (ordered-name=? (make-slip "yellow" "Wang" "Xi")
                              (make-slip "blue" "Wang" "Xi"))
              "Wang Xi should equal to Wang Xi when used ordered-name=?"))

;; reversed-name=? : Slip Slip -> Boolean
;; GIVEN: two Slips
;; RETURNS: true iff the two Slips have the reversed same name1 and name2
;; Strategy: use template of Slip
(define (reversed-name=? s1 s2)
  (and (string=? (slip-name1 s1) (slip-name2 s2))
       (string=? (slip-name2 s1) (slip-name1 s2))))
;; TEST
(begin-for-test
  (check-true (reversed-name=? (make-slip "yellow" "Xi" "Wang")
                               (make-slip "blue" "Wang" "Xi"))
              "Wang Xi should equal to Xi Wang when used reversed-name=?"))

;; name=? : Slip Slip -> Boolean
;; GIVEN: two Slips
;; RETURNS: true iff the two Slips have the same name
;; Strategy: combine simpler functions
(define (name=? s1 s2)
  (or (ordered-name=?  s1 s2)
      (reversed-name=? s1 s2)))
;; TEST
(begin-for-test
  (check-true  (name=? (make-slip "yellow" "Wang" "Xi")
                       (make-slip "yellow" "Xi" "Wang"))
               "name=? should return true for reversed named slips")
  (check-true  (name=? (make-slip "yellow" "Wang" "Xi")
                       (make-slip "yellow" "Wang" "Xi"))
               "name=? should reurn true for same slip")
  (check-false (name=? (make-slip "yellow" "Jones" "Tom")
                       (make-slip "yellow" "Wang" "Xi"))
               "name=? should return false for different names"))

;; color=? : Slip Slip -> Boolean
;; GIVEN: two Slips
;; RETURNS: true iff the two Slips have the same name
;; Strategy: use template for Slip
(define (color=? s1 s2)
  (string=? (slip-color s1) (slip-color s2)))
;; TEST
(begin-for-test
  (check-true  (color=? (make-slip "yellow" "Jones" "Tom")
                        (make-slip "yellow" "Wang" "Xi"))
               "color=? should return true for same colored slips")
  (check-false (color=? (make-slip "yellow" "Wang" "Xi")
                        (make-slip "blue" "Xi" "Wang"))
               "color=? should return false for different colored slips"))


;; contain? : Slip ListOfSlip -> Boolean
;; GIVEN: a Slip and a List of Slips
;; RETURNS: true iff the Slip is already in the list
;; Strategy: case on whether the list is empty
(define (contain? s los)
  (> (length (find-slip-in-list s los)) 0))
;; TEST
(begin-for-test
  (check-true  (contain? (make-slip "yellow" "Wang" "Xi") LIST1)
               "Wang should be contained in LIST1")
  (check-false (contain? (make-slip "blue" "Wang" "Xi") LIST1)
               "a blue Wang should not be contained in LIST1")
  (check-true  (contain? (make-slip "blue" "Luke" "Tom") LIST1)
               "a blue Luke Tom should be contained in LIST1"))


;; color-all-same? : ListOfSlip -> Boolean
;; GIVEN: a list of Slip
;; RETURNS: true iff every Slip in the list are of the same color
;; Strategy: use template for ListOfSlip and Slip
(define (color-all-same? los)
  (equal? (color-filter los (slip-color (first los)))
          los))
;; TEST
(begin-for-test
  (check-true  (color-all-same? LIST1-YELLOW)
               "YELLOW list should return true when called color-all-same?")
  (check-false (color-all-same? LIST1)
               "LIST1 should return false when called color-all-same?"))


;; remove-slip : Slip ListOfSlip -> ListOfSlip
;; RETURNS: the given List of Slip after the removal of the given Slip
;; Example: 
;;          => LIST1-WITHOUT-DUP
;; Strategy: use HOF filter on los
(define (remove-slip s los)
  (filter
   ;; Slip -> Boolean
   ;; RETURNS: true iff the given Slip is equal to the global Slip s
   (lambda (each-slip) (not (equal? s each-slip)))
   los))
;; TEST
(begin-for-test
  (check-equal? (remove-slip (make-slip "yellow" "Wang" "Xi") LIST1)
                LIST1-WITHOUT-DUP
                "after removing the first Wang, should get LIST1-WITHOUT-DUP"))


;; no-duplication? : ListOfSlip -> Boolean
;; RETURNS: true iff there are no duplications in the given list
;; Strategy: use HOF andmap on los
(define (no-duplication? los)
  (andmap
   ;; Slip -> Boolean
   ;; RETURNS: true iff the Slip is contained in los exactly once
   (lambda (each-slip)
     (not (contain? each-slip (remove-slip each-slip los))))
   los))
;; TEST
(begin-for-test
  (check-true (no-duplication? LIST1-WITHOUT-DUP)
              "no-duplication? on LIST1-WITHOUT-DUP should return true"))

;; duplication-filter : ListOfSlip -> ListOfSlip
;; GIVEN: a list of Slips
;; RETURNS: a list of Slips without duplications
;; Strategy: use HOF foldr on los
(define (duplication-filter los)
  (foldr
   ;; Slip ListOfSlip -> ListOfSlip
   ;; RETURNS: a ListOfSlip with the given Slip added
   ;;          if the Slip isn't already in it
   (lambda (s list-so-far)
     (if (contain? s list-so-far) list-so-far
         (cons s list-so-far)))
   empty
   los))
;; TEST
(begin-for-test
  (check-equal? (duplication-filter LIST1) LIST1-WITHOUT-DUP
                "duplication-filter should remove the first Wang in LIST1"))

;; felleisen-roster : ListOfSlip -> ListOfSlip
;; GIVEN: a list of Slips
;; RETURNS: a list of Slips containing all the students in Professor
;;         Felleisen's class, without duplication. (yellow)
;; Strategy: combine simpler functions
(define (felleisen-roster los)
  (duplication-filter (color-filter los "yellow")))
;; TEST
(begin-for-test
  (check-equal? (felleisen-roster LIST1) LIST1-FELLEISEN
                "felleisen-roster should return only Wang and K."))

;; shivers-roster: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;;         Shivers' class, without duplication. (blue)
;; Strategy: combine simpler functions
(define (shivers-roster los)
  (duplication-filter (color-filter los "blue")))
;; TEST
(begin-for-test
  (check-equal? (shivers-roster LIST1) LIST1-SHIVERS
                "shivers-roster should return only Jones and Luke"))

;; possible-roster? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff all the slips in the list are the same color,
;;                  and no student is represented twice.
;; Strategy: combine simpler functions
(define (possible-roster? los)
  (or (empty? los)
      (and (no-duplication? los)
           (color-all-same? los))))
;; TEST
(begin-for-test
  (check-equal? (possible-roster? LIST1-WITHOUT-DUP)
                false "possible-roster should reture false if not of same color")
  (check-equal? (possible-roster? LIST1-FELLEISEN)
                true "possible-roster should reture true for possible roster")
  (check-equal? (possible-roster? (shivers-roster LIST1)) true
                "possible-roster should return true for a roster"))


;; find-slip-in-list : Slip ListOfSlip -> ListOfSlip
;; GIVEN: a Slip and a list of Slip
;; RETURNS: all the instances of the given slip in the list
;; Strategy: use HOF filter on los
(define (find-slip-in-list s los)
  (filter (lambda (slip-in-list)
            (and (name=? s slip-in-list) (color=? s slip-in-list)))
          los))
;; TEST
(begin-for-test
  (check-equal? (length (find-slip-in-list WANG LIST1)) 2
                "LIST1 should contain 2 WANGs"))

;; exactly-one? : Slip ListOfSlip -> Boolean
;; GIVEN: a Slip and a list of Slip
;; RETURNS: true iff the Slip appears only once in the list
;; Strategy: combine simpler functions
(define (exactly-one? s los)
  (equal? (length (find-slip-in-list s los)) 1))
;; TEST
(begin-for-test
  (check-equal? (exactly-one? WANG LIST1-FELLEISEN) true
                "WANG should only appear once in roster"))


;; acceptable-felleisen-answer? : ListOfSlip ListOfSlip -> Boolean
;; GIVEN: two lists of slips, lst1 and lst2
;; RETURNS: true iff every student on a yellow slip in lst1 appears once
;;         and only once in lst2.
;; Strategy: case on whether the list is empty
(define (acceptable-felleisen-answer? lst1 lst2)
  (if (empty? lst1) true
      (and (or (not (color=? (first lst1) WANG))
               (exactly-one? (first lst1) lst2)) 
           (acceptable-felleisen-answer? (rest lst1) lst2))))
;; TEST
(begin-for-test
  (check-true (acceptable-felleisen-answer?
               EXAMPLE-LIST
               (list (make-slip "yellow" "Wang" "Xi")
                     (make-slip "yellow" "Shriram" "K.")))
               "example answers should be accepted as an acceptable-fellesien-answer")
  (check-true (acceptable-felleisen-answer?
               EXAMPLE-LIST
               (list (make-slip "yellow" "Shriram" "K.")
                     (make-slip "yellow" "Wang" "Xi")))
               "example answers should be accepted as an acceptable-fellesien-answer")
  (check-true (acceptable-felleisen-answer?
               EXAMPLE-LIST
               (list (make-slip "yellow" "Shriram" "K.")
                     (make-slip "yellow" "Xi" "Wang")))
               "example answers should be accepted as an acceptable-fellesien-answer")
  (check-true (acceptable-felleisen-answer?
               EXAMPLE-LIST
               (list (make-slip "yellow" "K." "Shriram")
                     (make-slip "yellow" "Xi" "Wang")))
               "example answers should be accepted as an acceptable-fellesien-answer"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CONSTANTS

(define WANG (make-slip "yellow" "Wang" "Xi"))

(define LIST1 (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "blue" "Jones" "Tom")
              (make-slip "blue" "Luke" "Tom")
              (make-slip "yellow" "Xi" "Wang")
              (make-slip "yellow" "Shriram" "K.")))

(define LIST1-WITHOUT-DUP
  (list
              (make-slip "blue" "Jones" "Tom")
              (make-slip "blue" "Luke" "Tom")
              (make-slip "yellow" "Xi" "Wang")
              (make-slip "yellow" "Shriram" "K.")))


(define LIST1-YELLOW (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "yellow" "Xi" "Wang")
              (make-slip "yellow" "Shriram" "K."))) 

(define LIST1-FELLEISEN (list
                         (make-slip "yellow" "Xi" "Wang")
                         (make-slip "yellow" "Shriram" "K.")))

(define LIST1-SHIVERS (list
                       (make-slip "blue" "Jones" "Tom")
                       (make-slip "blue" "Luke" "Tom")))

(define EXAMPLE-LIST (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "blue" "Jones" "Tom")
              (make-slip "yellow" "Xi" "Wang")
              (make-slip "yellow" "Shriram" "K.")))


                









