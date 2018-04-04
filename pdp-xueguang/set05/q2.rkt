;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)

(provide make-enrollment
         enrollment-student
         enrollment-class
         make-roster
         roster-classname
         roster-students
         behavior-correct?
         enrollments-to-rosters
         enrollments-to-rosters-bad-1
         enrollments-to-rosters-bad-2
         enrollments-to-rosters-bad-3
         )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A X can be anything that can be compared for equality with equal?

;; A Student can be anything that can be compared for equality with equal?

;; A Class can be anything that can be compared for equality with equal?

(define-struct enrollment (student class))
;; An EnrollmentAssertion is a (make-enrollment Student Class)
;; Template:
;; (define (enrollment-fn e)
;;   ...
;;   (enrollment-student e)
;;   (enrollment-class e))

(define-struct roster (classname students))
;; A ClassRosterAssertion is a (make-roster Class Student)
;; NOTE: the word ClassRoster, RosterAssertion and ClassRosterAssertion
;;       infer to the same thing, and are interchangable
;; Template:
;; (define (roster-fn r)
;;   ...
;;   (roster-classname r)
;;   (roster-students r))


;; A SetOfX is a list of X's without duplication.  Two SetOfX's are
;; considered equal if they have the same members.
;; Template:
;; (define (sox-fn sox)
;;   (if (empty? sox) ...
;;       (...(x-fn (first sox))
;;           (soe-fn (rest sox)))))


;; A SetOfEnrollmentAssertion is a list of EnrollmentAsertions without duplication
;; Two SetOfEnrollmentAssertions are considered equal if they have the same members.
;; Template:
;; (define (soe-fn soe)
;;   (if (empty? soe) ...
;;       (...(enrollment-fn (first soe))
;;           (soe-fn (rest soe)))))


;; A SetOfRosterAssertion is a list of RosterAssertions without duplication
;; Two SetOfRosterAssertions are considered equal if they have the same members.
;; Template:
;; (define (soa-fn soa)
;;   (if (empty? soa) ...
;;       (...(roster-fn (first soa))
;;           (soa-fn (rest soa)))))



;; A ProposedSolution is a function with contract:
;; SetOfEnrollmentAssertion -> SetOfClassRosterAssertion

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SET OPERATIONS

;; =====================================================
;; contain? : X SetofX -> Boolean
;; RETURNS: true iff the given X is contained in the given set of X
;; Strategy: use HOF ormap on sox
(define (contain? x sox)
  (ormap
   ;; X -> Boolean
   ;; RETURNS: true iff the given X is equal to the global X
   (lambda (each-x) (equal? x each-x))
   sox))
;; TEST
(begin-for-test
  (check-true  (contain? 4 (list 1 2 3 4 5))
               "4 is in the set, contain? should return true")
  (check-false (contain? 6 (list 1 2 3 4 5))
               "6 is not in the set, contain? should return false"))

;; =====================================================
;; set-contain? SetofX SetofX -> Boolean
;; RETURNS: true iff set1 contains set2
;;          which means every value in set2 can be found in set1
;; Strategy: use HOF andmap on set2
(define (set-contain? set1 set2)
  (andmap
   ;; X -> Boolean
   ;; RETURNS: true iff the given x in contained in the global set
   (lambda (x) (contain? x set1))
   set2))
;; TEST
(begin-for-test
  (check-true  (set-contain? (list 1 2 3) (list 1 2 3))
               "a set should contain itself")
  (check-true  (set-contain? (list 1 2 3) (list 1 2))
               "a set should contain its subset")
  (check-false (set-contain? (list 1 2 3) (list 3 4))
               "a set should not contain set with other values"))

;; =====================================================
;; set-equal? : SetofX SetofX -> Boolean
;; RETURNS: true iff the two given sets contains the exact same values
;; Strategy: combine simpler functions
(define (set-equal? set1 set2)
  (and (equal? (length set1) (length set2))
       (set-contain? set1 set2)))
;; TEST
(begin-for-test
  (check-true  (set-equal? (list 1 2 3) (list 1 2 3))
               "set-equal? should return true for same set")
  (check-false (set-equal? (list 1 2 3) (list 2 3))
               "set-equal? should return false for different set"))

;; roster-equal? : RosterAssertion RosterAssertion -> Boolean
;; RETURNS: true iff the two given RosterAssertions are equivalent
;; Strategy: use template of RosterAssertion
(define (roster-equal? r1 r2)
  (and (equal? (roster-classname r1) (roster-classname r2))
       (set-equal? (roster-students r1) (roster-students r2))))
;; TEST
(begin-for-test
  (check-true (roster-equal? (make-roster "Networks" (list "Kathryn" "Amy"))
                             (make-roster "Networks" (list "Amy" "Kathryn")))
              "reversed ordered student name should be accepted by roster-equal?")
  (check-false (roster-equal? (make-roster "Networks" (list "Kathryn" "Amy"))
                              (make-roster "AI" (list "Kathryn" "Amy")))
               "different classname should not be accepted by roster-equal?"))

;; =====================================================
;; roster-contain? : RosterAssertion SetOfRosterAssertion -> Boolean
;; RETURNS: true iff the given RosterAssertion is contained in the given Set
;; Strategy: use HOF ormap on sor
(define (roster-contain? r sor)
  (ormap
   (lambda (each-r) (roster-equal? r each-r))
   sor))
;; TEST
(begin-for-test (roster-contain?
                 (make-roster "PDP" (list "John" "Amy" "Feng"))
                 ROSTER1)
                "John, Amy, Feng in PDP should be contained in the ROSTER1")
  

;; =====================================================
;; set-of-roster-contain? : SetOfRosterAssertion SetOfRosterAssertion
;;                          -> Boolean
;; RETURNS: true iff sor1 contains sor2
;;          which means every RosterAssertion in sor1 can be found in sor2
;; Strategy: use HOF andmap on sor2
(define (set-of-roster-contain? sor1 sor2)
  (andmap
   ;; RosterAssertion -> Boolean
   ;; RETURNS: true iff given RosterAssertion is contained in the global set
   (lambda (r) (roster-contain? r sor1))
   sor2))
;; TEST
(begin-for-test
  (check-true  (set-of-roster-contain? ROSTER2 ROSTER1)
               "ROSTER2 should contain ROSTER1")
  (check-false (set-of-roster-contain? ROSTER1 ROSTER2)
               "ROSTER1 should not contain ROSTER2"))

;; =====================================================
;; set-of-roster-equal? : SetOfRosterAssertion SetOfRosterAssertion -> Boolean
;; RETURNS: true iff the two fiven sets contains the exact same values
;; Strategy: combine simpler function
(define (set-of-roster-equal? sor1 sor2)
  (and (equal? (length sor1) (length sor2))
       (set-of-roster-contain? sor1 sor2)))
;; TEST
(begin-for-test
  (check-true  (set-of-roster-equal? ROSTER1 ROSTER1)
               "a set should equal to itself")
  (check-false (set-of-roster-equal?
                (list
                 (make-roster "PDP" (list "John" "Feng" "Amy"))
                 (make-roster "Networks" (list "Kathryn" "Amy")))
                (list
                 (make-roster "AI" (list "John" "Feng" "Amy"))
                 (make-roster "Networks" (list "Kathryn" "Amy"))))
                "sets with different Class should not be equal")
  (check-false (set-of-roster-equal?
                (list
                 (make-roster "PDP" (list "John" "Feng" "Amy"))
                 (make-roster "Networks" (list "Kathryn" "Amy")))
                (list
                 (make-roster "PDP" (list "John" "Feng" "Anne"))
                 (make-roster "Networks" (list "Kathryn" "Amy"))))
                "sets with different ListOfStudent should not be equal"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPER FUNCTIONS


;; =====================================================
;; class-exist? : Class SetOfClassRoster -> Boolean
;; RETURNS: true iff the Class exists in the given set
;; Strategy: use HOF ormap on sor
(define (class-exist? c sor)
  (ormap
   ;; ClassRosterAssertion -> Boolean
   ;; RETURNS: true iff the given ClassRoster's Class
   ;;          is equal to the global Class c
   (lambda (each-r)
     (equal? (roster-classname each-r) c))
   sor))
;; TEST
(begin-for-test
  (check-true  (class-exist? "PDP" ROSTER1)
               "PDP should be in ROSTER1")
  (check-false (class-exist? "Math" ROSTER1)
               "Math should not be in ROSTER1"))


;; =====================================================
;; update-rosters : EnrollmentAssertion SetOfClassRoster -> SetOfClassRoster
;; RETURNS: a SetOfClassRoster with updated ClassRoster that
;;          have the same Class with the EnrollmentAssertion
;; Strategy: use HOF map on sor
(define (update-rosters e sor)
  (map
   ;; RosterAssertion -> RosterAssertion
   ;; RETURNS: updated RosterAssertion
   ;;          according to the given EnrollmentAssertion
   (lambda (r)
     (if (equal? (enrollment-class e) (roster-classname r))
         (make-roster (roster-classname r)
                      (cons (enrollment-student e) (roster-students r)))
         r))
   sor))
;; TEST
(begin-for-test
  (check-equal? (update-rosters PDP-WANG ROSTER1)
                ROSTER1-WITH-WANG
                "Wang should now in the ROSTER1 after update-rosters"))


;; =====================================================
;; add-new-class : EnrollmentAssertion SetOfClassRoster -> SetOfClassRoster
;; RETURNS: a SetOfClassRoster after adding a new ClassRoster
;;          according to the given EnrollmentAssertion
;; Strategy: use definition of SetOfClassRoster
(define (add-new-class e sor)
  (cons (make-roster (enrollment-class e)
                     (list (enrollment-student e)))
        sor))
;; TEST
(begin-for-test
  (check-true (set-of-roster-equal? (add-new-class AI-MING ROSTER1)
                                    ROSTER1-WITH-MING)
              "Ming should be in the ROSTER1 after add-new-class"))


;; =====================================================
;; add-enrollment-to-rosters : EnrollmentAssertion SetOfClassRoster
;;                             -> SetOfClassRoster
;; GIVEN: a EnrollmentAssertion and a SetOfClassRoster
;; RETURNS: the correct state of the SetOfClassRoster after inserting
;;          the given EnrollmentAssertion
;; Strategy: case on whether the class in alright in the set
(define (add-enrollment-to-rosters e sor)
  (if (class-exist? (enrollment-class e) sor)
      (update-rosters e sor)
      (add-new-class e sor)))
;; TEST
(begin-for-test
  (check-true (set-of-roster-equal?
               (add-enrollment-to-rosters PDP-WANG ROSTER1)
               ROSTER1-WITH-WANG)
              "WANG should now in ROSTER1 after add-enrollment-to-rosters")
  (check-true (set-of-roster-equal?
               (add-enrollment-to-rosters AI-MING ROSTER1)
               ROSTER1-WITH-MING)
              "MING should now in ROSTER1 after add-enrollment-to-rosters"))

  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DELIVERABLE FUNCTIONS

;; =====================================================
;; behavior-correct? : ProposedSolution SetOfEnrollmentAssertion -> Boolean
;; GIVEN: a ProposedSolution soln-fn and a SetOfEnrollmentAssertion se
;; RETURNS: true iff the output of soln-fn on se is an example of correct
;;          behavior by a ProposedSolution.
;; Strategy: combine simpler functions
(define (behavior-correct? ps soe)
  (set-of-roster-equal? (ps soe) (enrollments-to-rosters soe)))
;; TEST
(begin-for-test
  (check-true  (behavior-correct? enrollments-to-rosters ENROLLMENT1)
               "the enrollments-to-rosters function should be correct")
  (check-true  (behavior-correct? enrollments-to-rosters ENROLLMENT2)
               "the enrollments-to-rosters function should be correct")
  (check-false (behavior-correct? enrollments-to-rosters-bad-1 ENROLLMENT1)
               "the enrollments-to-rosters-bad-1 function should be incorrect")
  (check-false (behavior-correct? enrollments-to-rosters-bad-2 ENROLLMENT1)
               "the enrollments-to-rosters-bad-1 function should be incorrect")
  (check-false (behavior-correct? enrollments-to-rosters-bad-1 ENROLLMENT2)
               "the enrollments-to-rosters-bad-1 function should be incorrect")
  (check-false (behavior-correct? enrollments-to-rosters-bad-3 ENROLLMENT2)
               "the enrollments-to-rosters-bad-1 function should be incorrect"))



;; =====================================================
;; enrollments-to-rosters : SetOfEnrollmentAssertion -> SetOfClassRoster
;; GIVEN: a set of enrollments
;; RETURNS: a correct set of class rosters for the given enrollments
;; Strategy: use HOF foldr on soe
(define (enrollments-to-rosters soe)
  (foldr add-enrollment-to-rosters empty soe))
;; TEST
(begin-for-test
  (check-equal? (enrollments-to-rosters ENROLLMENT1)
                ROSTER1
                "The given example should hold")
  (check-true (set-of-roster-equal? (enrollments-to-rosters ENROLLMENT2)
                                    ENROLLMENT2-ROSTER)
              "The made up example should hold"))

;; =====================================================
;; enrollments-to-rosters-bad-1: SetOfEnrollmentAssertion -> SetOfClassRoster
;; GIVEN: a set of enrollment assertions
;; RETURN: an incorrect set of class rosters for the given enrollments.
;;         if the given set is empty, return a random SetOfClassRoster
;;         if it's not empty, return the correct answer without first element in it
;; Strategy: use template of SetOfClassRoster
(define (enrollments-to-rosters-bad-1 soe)
  (if (empty? soe) SOME-RANDOM-ROSTER1
   (rest (enrollments-to-rosters soe))))
;; TEST
(begin-for-test
  (check-false (behavior-correct? enrollments-to-rosters-bad-1 empty)
               "this function should fail correctness check when given empty list")
  (check-false (behavior-correct? enrollments-to-rosters-bad-1 ENROLLMENT1)
               "this function should not pass the correctness check"))

;; =====================================================
;; enrollments-to-rosters-bad-2: SetOfEnrollmentAssertion -> SetOfClassRoster
;; GIVEN: a set of enrollment assertions
;; RETURN: an incorrect set of class rosters for the given enrollments.
;;         if the given set is empty, return a random SetOfClassRoster
;;         if it's not empty, return an empty SetOfClassRoster
;; Strategy: use template of SetOfClassRoster
(define (enrollments-to-rosters-bad-2 soe)
  (if (empty? soe) SOME-RANDOM-ROSTER2
      empty))
;; TEST
(begin-for-test
  (check-false (behavior-correct? enrollments-to-rosters-bad-2 ENROLLMENT1)
               "this function should not pass the correctness check when given ENROLLMENT1")
  (check-false (behavior-correct? enrollments-to-rosters-bad-2 empty)
               "this function should not pass the correctness check when given empty list"))

;; enrollments-to-rosters-bad-3: SetOfEnrollmentAssertion -> SetOfClassRoster
;; GIVEN: a set of enrollment assertions
;; RETURN: an incorrect set of class rosters for the given enrollments.
;;         if the given set is empty, return a random SetOfClassRoster
;;         if it's not empty, return correct answer without the frist student
;; Strategy: use template of SetOfClassRoster
(define (enrollments-to-rosters-bad-3 soe)
  (if (empty? soe) SOME-RANDOM-ROSTER3
      (foldr add-enrollment-to-rosters empty (rest soe))))
;; TEST
(begin-for-test
  (check-false (behavior-correct? enrollments-to-rosters-bad-3 ENROLLMENT1)
               "this function should not pass the correctness check when given ENROLLMENT1")
  (check-false (behavior-correct? enrollments-to-rosters-bad-3 ENROLLMENT2)
               "this function should not pass the correctness check when given ENROLLMENT2")
  (check-false (behavior-correct? enrollments-to-rosters-bad-3 empty)
               "this function should not pass the correctness check when given empty set"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTING CONTANTS

(define ENROLLMENT1
  (list
   (make-enrollment "John" "PDP")
   (make-enrollment "Kathryn" "Networks")
   (make-enrollment "Feng" "PDP")
   (make-enrollment "Amy" "PDP")
   (make-enrollment "Amy" "Networks")))

(define ENROLLMENT2
  (list
   (make-enrollment "John" "PDP")
   (make-enrollment "Kathryn" "Networks")
   (make-enrollment "Feng" "PDP")
   (make-enrollment "Amy" "PDP")
   (make-enrollment "Amy" "Networks")
   (make-enrollment "Luke" "AI")
   (make-enrollment "Zheng" "PDP")))

(define ENROLLMENT2-ROSTER
  (list
   (make-roster "PDP" (list "John" "Feng" "Amy" "Zheng"))
   (make-roster "Networks" (list "Kathryn" "Amy"))
   (make-roster "AI" (list "Luke"))))

(define ROSTER1
  (list
   (make-roster "PDP" (list "John" "Feng" "Amy"))
   (make-roster "Networks" (list "Kathryn" "Amy"))))

(define SOME-RANDOM-ROSTER1 ROSTER1)

(define ROSTER2
  (list
   (make-roster "PDP" (list "John" "Feng" "Amy"))
   (make-roster "Networks" (list "Amy" "Kathryn"))
   (make-roster "AI" (list "Ming"))))

(define SOME-RANDOM-ROSTER2 ROSTER2)

(define PDP-WANG
  (make-enrollment "Wang" "PDP"))

(define AI-MING
  (make-enrollment "Ming" "AI"))

(define ROSTER1-WITH-WANG
  (list
   (make-roster "PDP" (list "Wang" "John" "Feng" "Amy"))
   (make-roster "Networks" (list "Kathryn" "Amy"))))

(define SOME-RANDOM-ROSTER3 ROSTER1-WITH-WANG)

(define ROSTER1-WITH-MING ROSTER2)

  