;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(provide
 program-to-strings
 make-def
 make-varexp
 make-appexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITION


;; A Variable is a Symbol

;; A ListOfX is one of
;; -- empty
;; -- (cons X ListOfX)
;; TEMPLATE
;; lox-fn : X -> ??
#;
(define (lox-fn lox)
  (if (empty? lox) ...
      (... (x-fn (first lox))
           (lox-fn (rest lox)))))

;; A ListOfVariable is one of
;; -- empty
;; (cons Variable ListOfVariable)
;; TEMPLATE
;; lov-fn : ListOfVariable -> ??
#;
(define (lov-fn lov)
  (cond
    [(empty? lov) ...]
    [else (... (first lov)
               (lov-fn last lov))]))


;; A Program is a ListOfDefinition.
;; TEMPLATE
;; program-fn : Program -> ??
#;
(define (program-fn p)
  (cond
    [(emtpy? p) ...]
    [else (definition-fn (first p))
          (program-fn (rest p))]))

;; A ListOfDefinition is one of:
;; -- empty
;; (cons Definition ListOfDefinition)
;; TEMPLATE
;; lod-fn : ListOfDefinition -> ??
#;
(define (lod-fn lod)
  (cond
    [(emtpy? lod) ...]
    [else (def-fn (first lod))
          (lod-fn (rest lod))]))




(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.
;; TEMPALTE
;; def-fn : Definition -> ??
#;
(define (def-fn d)
  (... (def-name d)
       (lov-fn (def-args d))
       (exp-fn (def-arg d))))


(define-struct varexp (name))
(define-struct appexp (fn args))
;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en
;; TEMPLATE
;; exp-fn : Exp -> ??
#;
(define (exp-fn e)
  (cond
    [(varexp? e) (... (var-exp-name e))]
    [(appexp? e) (... (app-fn (app-exp-fn e) (app-exp-args e)))]))



;; An Application is a (make-appexp Variable ListOfExpression)
;; INTERPRETATION:
;; v is the Variable that serves as the appliction's function call
;; lox is a list of expression that serves as the application's arguments
;; TEMPLATE
;; appexp-fn : Apppliciton -> ??
#;
(define (appexp-fn a)
  (... (appexp-v a)
       (loe-fn (appexp-loe a))))


;; A ListOfExpression is one of
;; -- empty
;; (cons Expression ListOfExpression)
;; TEMPLATE
;; loe-fn : ListOfExpression -> ??
#;
(define (loe-fn loe)
  (if (empty? loe) ...
      (... (exp-fn (first loe))
           (loe-fn (rest loe)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; program-to-strings : Program PosInt -> ListOfString
;; GIVEN: GarterSnake program and a width
;; RETURNS: a representation of the program as a sequence of lines,
;;         following the formatting rules described in the prompt.
;; Strategy: use HOF foldl on p
(define (program-to-strings p width)
  (foldr
   ;; Definition ListOfString -> ListOfString
   ;; RETURNS: a list of lines with the lines of the current Definition added
   (lambda (d los-sofar) (append (def-to-string d width 0) los-sofar))
   empty
   p))
;; TEST
(begin-for-test
  (check-equal?
   (program-to-strings sample-program 20)
   (list
    "def a-very-long-function-name (x) :"
    "    f1(x)"
    "def f2 (x,"
    "        a-very-long-variable-name,"
    "        y) :"
    "    f1(y)"
    "def f3 (x,z,t,u) :"
    "    f1(f2(z, y),"
    "       z,"
    "       f1(f2(z, y),"
    "          z))")
   "result should be same as shown for the sample program"))


;; def-to-string : Definition PosInt NonNegInt -> ListOfString
;; GIVEN: a list of definition, a width and indent indicator i
;; WHERE: i represents how many indents should the given definition at least has
;; RETURNS: a representation the definition as a sequence of lines
;; Strategy: combine simpler functions
(define (def-to-string d width i)
  (append (def-head-to-string (def-name d) (def-args d) width i)
          (exp-to-string (def-body d) width (+ i 4) true)))
;; TEST
(begin-for-test
  (check-equal?
   (def-to-string THIRD-EXAMPLE 20 0)
   (list "def f3 (x,z,t,u) :"
         "    f1(f2(z, y),"
         "       z,"
         "       f1(f2(z, y),"
         "          z))")
   "result should be same as shown in the third example"))


;; def-head-to-string : Variable ListOfVariable PosInt NonNegInt -> ListOfString
;; GIVEN: a Variable, a list of Variable, a width and a indetn indicator i
;; WHERE: i represents how many indents should the given definition at least has
;; RETURNS: a representation the head as the sequence of lines
;; Strategy: 
(define (def-head-to-string name args width i) 
  (if (or (equal? (length args) 1) (head-width-fits name args width))
      (list
       (string-append (indent i true)"def " (symbol->string name) " ("
                      (list-of-var-to-string args) ") :"))
      (head-to-lines name args i)))
;; TEST
(begin-for-test
  (check-equal?
   (def-head-to-string 'a-very-long-function-name (list 'x) 20 0)
   (list "def a-very-long-function-name (x) :")
   "if the width fits in one line, output should be in one line")
  (check-equal?
   (def-head-to-string 'f2 (list 'x 'a-very-long-variable-name 'y) 20 0)
   (list "def f2 (x,"
         "        a-very-long-variable-name,"
         "        y) :")
   "if the width does not fit in one line, break the arguments into lines."))


;; head-to-lines : Variable ListOfVariable NonNegInt -> ListOfString
;; GIVEN: a Variable, a list of Variable, a width and a indetn indicator i
;; WHERE: i represents how many indents should the given definition at least has
;;        and the head should be break into different lines
;;        the length of args is >= 1
;; RETURNS: a representation the head as the sequence of lines
;; Strategy:
(define (head-to-lines name args i)
  (cons
   (string-append "def " (symbol->string name) " ("
                  (symbol->string (first args)) ",")
   (arguments-to-lines
    (reverse (rest args))
    (+ 6 (var-len name) i))))
;; TEST
(begin-for-test
  (check-equal? (head-to-lines 'f2 (list 'x 'a-very-long-variable-name 'y) 0)
                (list "def f2 (x,"
                      "        a-very-long-variable-name,"
                      "        y) :")
                "break the arguments into lines no matter what"))

;; arguments-to-lines : ListOfVariable NonNegInt -> ListOfString
;; GIVEN: a list of variable and a indent indicator
;; WHERE: the list should be (reverse (rest list0)) of some argument list list0
;; WHERE: i represents how many indents should the given list at least has
;; RETURNS: a representation of strings with indent specified
;; Strategy: use HOF foldr on args
(define (arguments-to-lines args i)
  (foldr
   ;; Variable ListOfString -> ListOfString
   ;; RETURNS: a list of string representing the arguments
   (lambda (arg sofar)
     (cons (string-append (var-to-string arg i true) ",") sofar))
   (list (string-append (var-to-string (first args) i true) ") :"))
   (rest args)))
;; TEST
(begin-for-test
  (check-equal?
   (arguments-to-lines (reverse (list  'a-very-long-variable-name 'y)) 8)
   (list "        a-very-long-variable-name," "        y) :")
   "the remaining arguments should be break into lines."))

;; head-width-fits : Variable ListOfVariable PosInt-> Boolean
;; GIVEN: a Variable, a list of Variable and a width
;; RETURNS: true iff length of the given head is <= width
;; Strategy: combine simpler functions
(define (head-width-fits name args width)
  (<= (+ 9
         (string-length (symbol->string name))
         (string-length (list-of-var-to-string args)))
      width))
;; TEST
(begin-for-test
  (check-false
   (head-width-fits 'a-very-long-function-name (list 'x) 20)
   "the first function head in the example should fit under width 20")
  (check-true
   (head-width-fits 'f3 (list 'x 'z 't 'u) 20)
   "the thrid function head in the example should fit under width 20"))

;; list-of-var-to-string : ListOfVariable -> String
;; GIVEN: a list of variables
;; WHERE: the list is not empty 
;; RETURNS: the stirng of those variables combined
;; Strategy: use HOF foldr on lov
(define (list-of-var-to-string lov)
  (if (empty? lov) ""
      (foldl
       (lambda (v sofar)
         (string-append sofar "," (symbol->string v)))
       (symbol->string (first lov))
       (rest lov))))
;; TEST
(begin-for-test
  (check-equal?
   (list-of-var-to-string (list 'x 'z 't 'u))
   "x,z,t,u"
   "list-of-var-to-string should convert list to string and add commas")
  (check-equal?
   (list-of-var-to-string (list 'x))
   "x"
   "should convert single value list to string and add commas")
  (check-equal? (list-of-var-to-string empty) ""
                "should return empty"))

;; exp-to-string : Expression PosInt NonNegInt Boolean-> ListOfString
;; GIVEN: an expression, a width, a indent indicator i, a newline indicator
;; WHERE: i represents how many indents should the given expression at least has
;; WHERE: new-line? indicates whether this exp starts on a new line
;; RETURNS: a list of string representing exp0
;; Strategy: case on whether the given expression is a varexp or a appexp
;; Haulting Measure: the length of descendants of the given expression
(define (exp-to-string exp width i new-line?)
  (cond
    [(varexp? exp) (list (var-to-string (varexp-name exp) i new-line?))]
    [(appexp? exp) (appexp-to-string exp width i new-line?)]))
;; TEST
(begin-for-test
  (check-equal?
   (exp-to-string (make-appexp 'f1 (list (make-varexp 'y)))
                  20 4 true)
   (list "    f1(y)")
   "result should be same as shown in example 2"))


;; appexp-to-string : Application PosInt NonNegInt Boolean -> ListOfString
;; GIVEN: an expression exp, a width, an indent indicator i
;;        a newline indicator 
;; WHERE: i represents how many indents should the given expression at least has
;; WHERE: new-line? indicates whether this app starts on a new line
;; RETURNS: a list of string representing given application
;; Strategy: case on whether the whole application fits on one line
;; Haulting Measure: the length of descendants of the given application
(define (appexp-to-string app width i new-line?)
  (cond
    [(fits? (appexp-one-line app 0) width i)
     (list (string-append (indent i new-line?) (appexp-one-line app 0)))]
    [else (cons-same-line (var-to-string (appexp-fn app) i new-line?)
                          (args-to-string app width i))]))
;; TEST
(begin-for-test
  (check-equal?
   (appexp-to-string THIRD-BODY 20 4 true)
   (list "    f1(f2(z, y),"
         "       z,"
         "       f1(f2(z, y),"
         "          z))")
   "result should be same as shown in example 3"))

;; args-to-string : ListOfExpression PosInt NonNegInt -> ListOfString
;; GIVEN: a list of expression, a width, an indent indicator,
;;        a newline indicator 
;; WHERE: i represents how many indents should the given expression at least has
;; RETURNS: a list of string representing the given list of exp
;; Strategy: case on whether the first arg fits on the line
;; Haulting Measure: the length of descendants of the given list of expression
(define (args-to-string app width i)
  (cond
    [(first-argument-fits? app width i)
     (cons-same-line "("
                     (list-of-exp-to-string (appexp-args app) width
                                            (+ i (var-len (appexp-fn app)) 1)))]
    [else (append-same-line
           (list "" (string-append (indent (+ i 1) true) "("))
           (list-of-exp-to-string (appexp-args app) width (+ i 2)))]))
;; TEST
(begin-for-test
  (check-equal?
   (args-to-string
    (make-appexp 'test empty) 5 4)
   (list "" "     ()")
   "when args is empyt, line won't fit, should return new-line and empty parens")
  
  (check-equal?
   (args-to-string
    (make-appexp 't
                 (list (make-varexp 'z) (make-varexp 'y)
                       (make-appexp 'f2 (list (make-varexp 'z)))))
    20 4)
   (list "(z,"
         "      y,"
         "      f2(z))")
   "when first argument would fit on the line, should not return a new line."))


;; first-argument-fits? : Expressino PosInt NonNegInt -> Boolean
;; GIVEN: an exp, a width, an indent indicator  
;; WHERE: i represents how many indents should the given expression at least has
;; RETURNS: true iff the first line of given exp will fit on the line
;; Strategy: use template for LOE on (appexp-args app)
;; Haulting Measure: the length of descendants of the given expression
(define (first-argument-fits? app width i)
  (fits?
   (if (empty? (appexp-args app)) "()"
       (first (exp-to-string (first (appexp-args app)) width i false)))
   width i))
;; TEST
(begin-for-test
  (check-true
   (first-argument-fits?
    (make-appexp 't
                 (list (make-varexp 'z) (make-varexp 'y)
                       (make-appexp 'f2 (list (make-varexp 'z)))))
    20 4)
   "'    t(z,' should be able to fit in line"))

;; list-of-exp-to-string : ListOfExpression PosInt NonNegInt -> ListOfString
;; GIVEN: a list of expression, a width, an indent indicator, a newline indicator
;; WHERE: each exp in the list should be placed on seperate lines
;; WHERE: i represents how many indents should the given expression at least has
;; RETURNS: a list of string representing the given list of exp
;; Strategy: use template for LOE on loe
;; Haulting Measure: the length of descendants of the given list of expression
(define (list-of-exp-to-string loe width i)
  (add-closing-paren
   (if (empty? loe) (list "")
       (append
        (argument-to-string (first loe) width i false (empty? (rest loe)))
        (rest-list-of-arg-to-string (rest loe) width i)))))
;; TEST
(begin-for-test
  (list-of-exp-to-string
   (list (make-varexp 'z) (make-varexp 'y)
         (make-appexp 'f2 (list (make-varexp 'z))))
   20 4)
  (list "z,"
        "    y,"
        "    f2(z)")
  "the first arg should not be on new line")

;; rest-list-of-arg-to-string : ListOfExpression PosInt NonNegInt
;;                              -> ListOfString
;; GIVEN: a list of expression, a width, an indent indicator,
;;        a newline indicator
;; WHERE: each exp in the list should be placed on seperate lines
;; WHERE: the given LOE is (rest loe0), where loe0 is the whole argument list 
;; WHERE: i represents how many indents should the given expression at least has
;; RETURNS: a list of string representing the rest of the loe0
;; Strategy: use template for LOE on loe
;; Haulting Measure: the length of descendants of the given list of exp
(define (rest-list-of-arg-to-string args width i)
  (if (empty? args) empty
      (append
       (argument-to-string (first args) width i true (empty? (rest args)))
       (rest-list-of-arg-to-string (rest args) width i))))
;; TEST
(begin-for-test
  (check-equal?
   (rest-list-of-arg-to-string
    (list (make-varexp 'z) (make-varexp 'y)
          (make-appexp 'f2 (list (make-varexp 'z))))
    20 4)
   (list "    z,"
         "    y,"
         "    f2(z)")
   "every arg should be on new line."))


;; argument-to-string : Expression PosInt NonNegInt Boolean -> ListOfString
;; GIVEN: an exp, a width, an indent indicator, a last-line indicator
;; WHERE: i represents how many indents should the given expression at least has
;; WHERE: new-line? indicates whether this app starts on a new line
;; WHERE: rest-empty? represents whether the given argument is the last one
;;        in some LOE loe0
;; RETURNS: a list of string representing the given argument
;; Haulting Measure: the length of descendants of the givne exp
;; Strategy: use combine simpler functions
(define (argument-to-string arg width i new-line? rest-empty?)
  (add-closing-comma
   (exp-to-string arg width i new-line?) (not rest-empty?)))
;; TEST
(begin-for-test
  (argument-to-string
   (make-appexp 'f2 (list (make-varexp 'z) (make-varexp 'y)))
   20 7 false false)
  (list "f2(z, y),")
  "argument-to-string should return the arg in a list")


;; appexp-one-line : Application NonNegInt -> String
;; GIVEN: an application expression,  an indent indicator i
;; WHERE: i represents how many indents should the given expression at least has
;; RETURNS: a string where the whole application expression on one line
;; Strategy: case on whether the first argument is varexp or appexp
(define (appexp-one-line app i)
  (string-append
   ;; the function head
   (var-to-string (appexp-fn app) i true) "("
   ;; first argument
   (if (varexp? (first (appexp-args app)))
       (var-to-string (varexp-name (first (appexp-args app))) 0 false)
       (appexp-one-line (first (appexp-args app)) 0))
   ;; rest argument
   (list-of-exp-one-line (rest (appexp-args app)))
   ;; closing paren
   ")"))
;; TEST
(begin-for-test
  (check-equal?
   (appexp-one-line
    (make-appexp
     'f1
     (list 
      (make-appexp 'f2 (list (make-varexp 'z) (make-varexp 'y)))))
    4)
   "    f1(f2(z, y))"
   "appexp-one-line should return everything in one line"))

;; list-of-exp-one-line : ListOfExpression -> String
;; GIVEN: a list of expressions
;; RETURNS: the string that represents these expressions
;; Strategy: use template For ListOfExpressions on loe
(define (list-of-exp-one-line loe)
  (cond
    [(empty? loe) ""]
    [(varexp? (first loe))
     (string-append ", " (var-to-string (varexp-name (first loe)) 0 true) 
                    (list-of-exp-one-line (rest loe)))]
    [(appexp? (first loe))
     (string-append ", " (appexp-one-line (first loe) 0) 
                    (list-of-exp-one-line (rest loe)))]))
;; TEST
(begin-for-test
  (check-equal?
   (list-of-exp-one-line
    (list (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y)))
          (make-appexp 'f2 (list (make-varexp 'm) (make-varexp 'n)))))
   ", f1(x, y), f2(m, n)"
   "list-of-exp-one-line should return comma sepreated value on one line"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; HELPER FUNCTIONS

;; cons-same-line : String ListOfString -> ListOfString
;; GIVEN: a string and a list of string
;; RETURNS: the list of the string with the string added on the top
;; Strategy: use template for ListOfString on los
(define (cons-same-line s los)
  (if (empty? los) (list s)
      (cons (string-append s (first los))
            (rest los))))
;; TEST
(begin-for-test
  (check-equal? (cons-same-line "fn(" (list "x," "y"))
                (list "fn(x," "y")
                "the string should be added to the first of the list")
  (check-equal? (cons-same-line "x" empty) (list "x")
                "Adds 'x' to the list"))


;; append-same-line : ListOfString ListOfString -> ListOfString
;; GIVEN: 2 lists of string los1 and los2
;; RETURNS: the lists of string combined with the last element of the los1
;;          added to the first element of los2
;; EXAMPLE: (append-same-line (list "1" "2") (list "2" "3"))
;;            => (list "1" "22" "3")
;; Strategy: use template on ListOfString on los1 and los2
(define (append-same-line los1 los2)
  (append (except-last los1)
          (cons-same-line (last los1) los2)))
;; TEST
(begin-for-test
  (check-equal? (append-same-line (list "1" "2") (list "2" "3"))
                (list "1" "22" "3")
                "the 2 in los1 and los2 should be added together"))


;; add-x-at-last-element : ListOfStirng String -> ListOfString
;; GIVEN: a list of string and a string
;; RETURNS: the list of string with given string added to the last element
;; Strategy: use template for ListOfString on los
(define (add-x-at-last-element los s)
  (append (except-last los)
          (list (string-append (last los) s))))
;; TEST
(begin-for-test
  (check-equal? (add-x-at-last-element (list "0" "a" "b" "c") ")")
                (list "0" "a" "b" "c)")
                "should return the c with an extra paren."))

;; last : ListOfX -> X
;; GIVEN: a list of X
;; WHERE: the length of the given list >= 1
;; RETURN: the last element of the X
;; Strategy: use template for LOX on lox
(define (last lox)
  (first (reverse lox)))
;; TEST
(begin-for-test
  (check-equal? (last (list 1 2 3))
                3 "last should return the last element"))

;; except-last : ListOfX -> ListOfX
;; GIVEN: a list of X
;; WHERE: the length of the given list >= 1
;; RETURNS: all the elements except the last element
;; Strategy: use template for ListOfX on lox
(define (except-last lox)
  (reverse (rest (reverse lox))))
;; TEST
(begin-for-test
  (check-equal? (except-last (list 1 2 3))
                (list 1 2)
                "except-last should exclude element 3"))

;; add-comma-and-closing-paren : ListOfString -> ListOfString
;; GIVEN: a list of string
;; RETURNS: the list of string with comma added to each string,
;;          and a closing paren added to the last string
;; Strategy: use HOF map on (except-last los)
(define (add-comma-and-closing-paren los)
  (append (map (lambda (s) (string-append s ",")) (except-last los))
          (list (string-append (last los) ")"))))
;; TEST
(begin-for-test
  (check-equal? (add-comma-and-closing-paren (list "a" "b" "c"))
                (list "a," "b," "c)")
                "a,b should be added comma while c should be added paren."))


;; add-closing-paren : ListOfString -> ListOfString
;; GIVEN: a list of string
;; RETURNS: the list of string with a paren added to the first element
;; Strategy: call a more general function
(define (add-closing-paren los)
  (add-x-at-last-element los ")"))
;; TEST
(begin-for-test
  (check-equal? (add-closing-paren (list "1" "a" "b"))
                (list "1" "a" "b)")
                "add-closing-paren should add paren to the last element."))


;; add-closing-comma : ListOfString Boolean -> ListOfString
;; GIVEN: a list of string and a boolean
;; RETURNS: the list of string with a comma added to the first element
;;          iff the given boolean is true
;; Strategy: call a more general function
(define (add-closing-comma los b)
  (if b (add-x-at-last-element los ",") los))
;; TEST
(begin-for-test
  (check-equal? (add-closing-comma (list "a" "b") false)
                (list "a" "b")
                "if given false, the given list should be returned unchanged")
  (check-equal? (add-closing-comma (list "a" "b") true)
                (list "a" "b,")
                "if given true, the first element should be added a comma"))

;; fits? : String NonNegInt PosInt -> Boolean
;; GIVEN: a string, a width, a indent indicator
;; RETURNS: true iff the string fits in the line
;; Strategy: combine simpler functions
(define (fits? s width i)
  (<= (+ (string-length s) i) width))
;; TEST
(begin-for-test
  (check-true (fits? "a" 10 8)
              "a lenght 1 should be fit into width 10, indent 8")
  (check-false (fits? "hello" 10 8)
               "a length 5 should not fit into width 10, indent 8"))

;; var-to-string : Variable/Expression PosInt Boolean -> String
;; GIVEN: a Variable/varexp and a indent indicator
;;        a newline indicator 
;; WHERE: i represents how many indents should the given list at least has
;; RETURNS: a representation of the variable with indent specified
;; Strategy: case on whether the given var is Variable or varexp
(define (var-to-string var i new-line?)
  (if (symbol? var)
      (string-append (indent i new-line?) (symbol->string var))
      (string-append (indent i new-line?) (symbol->string (varexp-name var)))))
;; TEST
(begin-for-test
  (check-equal? (var-to-string 'a 3 true) "   a"
                "var-to-string should return the symbol with its indent")
  (check-equal? (var-to-string (make-varexp 'a) 3 true) "   a"
                "var-to-string should return the varexp with its indent"))


;; indent : PosInt Boolean -> String
;; RETURNS: a string consists of spaces according to the given PosInt
;;          a newline indicator 
;; Strategy: combine simpler functions
(define (indent i new-line?)
  (if new-line? (make-string i #\space) ""))
;; TEST
(begin-for-test
  (check-equal? (indent 5 true) "     " "should return 5 spaces")
  (check-equal? (indent 10 false ) ""
                "when given false, should return empty string"))



;; var-len : Variable -> Integer
;; RETURNS: the length of the given Variable
;; Strategy: combine simpler functions
(define (var-len v)
  (string-length (symbol->string v)))
;; TEST
(begin-for-test
  (check-equal? (var-len 'something) 9
                "the length of 'something' should be 9"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TEST CONSTANTS
(define sample-program
  (list
   (make-def 'a-very-long-function-name
             (list 'x)
             (make-appexp 'f1 (list (make-varexp 'x))))
   (make-def 'f2 (list 'x 'a-very-long-variable-name 'y)
             (make-appexp 'f1 (list (make-varexp 'y))))
   (make-def 'f3 (list 'x 'z 't 'u)
             (make-appexp
              'f1
              (list (make-appexp
                     'f2 (list (make-varexp 'z)
                               (make-varexp 'y)))
                    (make-varexp 'z)
                    (make-appexp
                     'f1 (list (make-appexp
                                'f2 (list (make-varexp 'z)
                                          (make-varexp 'y)))
                               (make-varexp 'z))))))))


(define FIRST-EXAMPLE (first sample-program))
(define SECOND-EXAMPLE (second sample-program))
(define THIRD-EXAMPLE (third sample-program))
(define FIRST-BODY (def-body FIRST-EXAMPLE))
(define SECOND-BODY (def-body SECOND-EXAMPLE))
(define THIRD-BODY (def-body THIRD-EXAMPLE))

(define TEST-APP (make-appexp
                  'f1
                  (list (make-appexp
                         'f2 (list (make-varexp 'z)
                                   (make-varexp 'y)))
                        (make-varexp 'first)
                        (make-appexp
                         'f1 (list (make-appexp
                                    'f2 (list (make-varexp 'z)
                                              (make-varexp 'y)))
                                   (make-varexp 'second))))))




