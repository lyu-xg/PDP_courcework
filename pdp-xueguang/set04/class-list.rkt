;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname class-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

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
;; ;; (define (los-fn loc)
;;   (if (empty? los) (...)
;;       (... (first los) (rest los)...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTIONS

;; color-filter : ListOfSlip Color -> ListOfSlip
;; GIVEN: a list of Slips
;; RETURNS: a list of Slips containing all the Slips with the given number
;; Strategy: case on whether the list is empty
(define (color-filter los c)
  (if (empty? los) empty
      (if (string=? (slip-color (first los)) c)
          (cons (first los) (color-filter (rest los) c))
          (color-filter (rest los) c))))

;; ordered-name=? : Slip Slip -> Boolean
;; GIVEN: two Slips
;; RETURNS: true iff the two Slips have the same name1 and name2
;; Strategy: combine simpler functions
(define (ordered-name=? s1 s2)
  (and (string=? (slip-name1 s1) (slip-name1 s2))
       (string=? (slip-name2 s1) (slip-name2 s2))))

;; reversed-name=? : Slip Slip -> Boolean
;; GIVEN: two Slips
;; RETURNS: true iff the two Slips have the reversed same name1 and name2
;; Strategy: combine simpler functions
(define (revsered-name=? s1 s2)
  (and (string=? (slip-name1 s1) (slip-name2 s2))
           (string=? (slip-name2 s1) (slip-name1 s2))))

;; name=? : Slip Slip -> Boolean
;; GIVEN: two Slips
;; RETURNS: true iff the two Slips have the same name
;; Strategy: combine simpler functions
(define (name=? s1 s2)
  (or (ordered-name=?  s1 s2)
      (revsered-name=? s1 s2)))

;; color=? : Slip Slip -> Boolean
;; GIVEN: two Slips
;; RETURNS: true iff the two Slips have the same name
;; Strategy: use template for Slip
(define (color=? s1 s2)
  (string=? (slip-color s1) (slip-color s2)))

;; contain? : Slip ListOfSlip -> Boolean
;; GIVEN: a Slip and a List of Slips
;; RETURNS: true iff the Slip is already in the list
;; Strategy: case on whether the list is empty
(define (contain? s los)
  (> (length (find-slip-in-list s los)) 0))


;; color-all-same? : ListOfSlip -> Boolean
;; GIVEN: a list of Slip
;; RETURNS: true iff every Slip in the list are of the same color
;; Strategy: case on whether the length of the list is less or equal to 1
(define (color-all-same? los)
  (if (<= (length los) 1) true
      (and (color=? (first los) (first (rest los)))
           (color-all-same? (rest los)))))


;; no-duplication? : ListOfSlip -> Boolean
;; RETURNS: tree iff there are no duplications in the given list
;; Strategy: case on whether the list is empty
(define (no-duplication? los)
  (if (empty? los)
      true
      (and (not (contain? (first los) (rest los)))
           (possible-roster? (rest los)))))


;; duplication-filter : ListOfSlip -> ListOfSlip
;; GIVEN: a list of Slips
;; RETURNS: a list of Slips without duplications
;; Strategy: case on whether the 
(define (duplication-filter los)
  (cond
    [(empty? los) empty]
    [(contain? (first los) (rest los))
     (duplication-filter (rest los))]
    [else
     (cons (first los) (duplication-filter (rest los)))]))


;; felleisen-roster : ListOfSlip -> ListOfSlip
;; GIVEN: a list of Slips
;; RETURNS: a list of Slips containing all the students in Professor
;;         Felleisen's class, without duplication. (yellow)
;; Strategy: combine simpler functions
(define (felleisen-roster los)
  (duplication-filter (color-filter los "yellow")))


;; shivers-roster: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;;         Shivers' class, without duplication. (blue)
;; Strategy: combine simpler functions
(define (shivers-roster los)
  (duplication-filter (color-filter los "blue")))

;; possible-roster? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff all the slips in the list are the same color,
;;                  and no student is represented twice.
;; Strategy: case on whether the list is empty or not
(define (possible-roster? los)
  (and (no-duplication? los) (color-all-same? los)))

;; find-slip-in-list : Slip ListOfSlip -> ListOfSlip
;; GIVEN: a Slip and a list of Slip
;; RETURNS: all the instances of the given slip in the list
;; Strategy: case on whether the list is empty
(define (find-slip-in-list s los)
  (if (empty? los) empty
      (if (and (name=? s (first los)) (color=? s (first los)))
          (cons (first los) (find-slip-in-list s (rest los)))
          (find-slip-in-list s (rest los)))))

;; exactly-one? : Slip ListOfSlip -> Boolean
;; GIVEN: a Slip and a list of Slip
;; RETURNS: true iff the Slip appears only once in the list
;; Strategy: combine simpler functions
(define (exactly-one? s los)
  (equal? (length (find-slip-in-list s los)) 1))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(begin-for-test
  (check-equal? (color-filter LIST1 "yellow") LIST1-YELLOW
                "color filter should filter out the blue slips")
  (check-equal? (name=? (make-slip "yellow" "Wang" "Xi")
                        (make-slip "yellow" "Xi" "Wang"))
                true "name=? should return true for reversed named slips")
  (check-equal? (name=? (make-slip "yellow" "Wang" "Xi")
                        (make-slip "yellow" "Wang" "Xi"))
                true "name=? should reurn true for same slip")
  (check-equal? (name=? (make-slip "yellow" "Jones" "Tom")
                        (make-slip "yellow" "Wang" "Xi"))
                false "name=? should return false for different names")
  (check-equal? (contain? (make-slip "yellow" "Wang" "Xi") LIST1)
                true
                "Wang should be contained in LIST1")
  (check-equal? (contain? (make-slip "blue" "Wang" "Xi") LIST1)
                false
                "a blue Wang should not be contained in LIST1")
  (check-equal? (contain? (make-slip "blue" "Luke" "Tom") LIST1)
                true
                "reverse ordered name should return true")
  (check-equal? (duplication-filter LIST1) LIST1-WITHOUT-DUP
                "duplication-filter should remove the first Wang in LIST1")
  (check-equal? (felleisen-roster LIST1) LIST1-FELLEISEN
                "felleisen-roster should return only Wang and K.")
  (check-equal? (possible-roster? LIST1-WITHOUT-DUP)
                false "possible-roster should reture false if not of same color")
  (check-equal? (possible-roster? LIST1-FELLEISEN)
                true "possible-roster should reture true for possible roster")
  (check-equal? (shivers-roster LIST1) LIST1-SHIVERS
                "shivers-roster should return only Jones and Luke")
  (check-equal? (possible-roster? (shivers-roster LIST1)) true
                "possible-roster should return true for a roster")
  (check-equal? (length (find-slip-in-list WANG LIST1)) 2
                "LIST1 should contain 2 WANGs")
  (check-equal? (exactly-one? WANG LIST1-FELLEISEN) true
                "WANG should only appear once in roster")
  (check-equal? (acceptable-felleisen-answer?
                 EXAMPLE-LIST
                 (list (make-slip "yellow" "Wang" "Xi")
                       (make-slip "yellow" "Shriram" "K.")))
                true "example answers should be accepted as an acceptable-fellesien-answer")
  (check-equal? (acceptable-felleisen-answer?
                 EXAMPLE-LIST
                 (list (make-slip "yellow" "Shriram" "K.")
                       (make-slip "yellow" "Wang" "Xi")))
                true "example answers should be accepted as an acceptable-fellesien-answer")
  (check-equal? (acceptable-felleisen-answer?
                 EXAMPLE-LIST
                 (list (make-slip "yellow" "Shriram" "K.")
                       (make-slip "yellow" "Xi" "Wang")))
                true "example answers should be accepted as an acceptable-fellesien-answer")
  (check-equal? (acceptable-felleisen-answer?
                 EXAMPLE-LIST
                 (list (make-slip "yellow" "K." "Shriram")
                       (make-slip "yellow" "Xi" "Wang")))
                true "example answers should be accepted as an acceptable-fellesien-answer")
)
                









