;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lopez-Lab3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;Jonathan Lopez, Tyler Sanderville
;;jrlopez, tasanderville
;;jrlopez@wpi.edu
;;tasanderville@wpi.edu

;;group 1072

;;HW 3



;;problem 1

; make-borrower: String String String Number Number -> Borrower
(define-struct borrower (name origin-country business request-loan percentage))

;;a Borrower is a (make-borrower String String String Number Number)
;;interpretation represents a Borrower where
;; 	name is the name of the borrower
;;  	origin-country is the country if origin of the borrower
;;  	business is the business is the kind of business the funds are needed for
;;  	request-loan is the funds requested
;;  	percentage is the percentage the loan raised so far


;; 3 examples
(define BORROWER1 (make-borrower "Tyler" "United States" "Construction" 12000 .10))
(define BORROWER2 (make-borrower "Jon" "Canada" "Aviation" 1000 .30))
(define BORROWER3 (make-borrower "Joe" "Great Britain" "IT" 5000 .20))


;;template
;; borrower-fcn: Borrower ->

#;(define (borrower-fcn a-borrower)
   (... (borrower-name a-borrower)
    	(borrower-origin-country a-borrower)
    	(borrower-business a-borrower)
    	(borrower-request-loan a-borrower)
    	(borrower-percentage a-borrower)))



;;Problem 2


;; a ListOfBorrower is one of
;;  empty
;;  (cons String Number ListOfBorrower)
;; interp:  ListOfBorrower represents a list of Borrowers

;;example
(define BORROWERS (cons BORROWER1 (cons BORROWER2 (cons BORROWER3 empty))))


;;template

;;lob-fcn: ListofBorrower ->

#;(define (lob-fcn alob)
  (cond [(empty? alob) (...) ]
    	[(cons? alob) (... (borrower-fcn (first alob))
                       	(lob-fcn (rest alob)))]))

;;Problem 3

;;any-from?: ListOfBorrower String-> Boolean
;;Consumes a LoB and a string and produces true if any strings in
;; borrower-origin-country match the input string

(define (any-from? alob country)
  (cond [(empty? alob) false]
    	[(cons? alob) (if (help-any-from? (first alob) country)
                          true
                          (any-from? (rest alob) country))]))
                       

;;Helper Function
;;help-any-from?: a-borrower, String-> Boolean
;; This functions consumes a borrower and a string and produces true if the
;; origin country of a-borrower

(define (help-any-from? a-borrower country)
   (if  (string=? country (borrower-origin-country a-borrower))
    	true
    	false
    	))

;;tests for any-from

(check-expect (any-from? BORROWERS "Canada") true)
(check-expect (any-from? (cons BORROWER2 (cons BORROWER3 empty)) "United States") false)
(check-expect (any-from? empty "") false)

;;tests for helper function

(check-expect (help-any-from? BORROWER2 "Canada") true)
(check-expect (help-any-from? BORROWER2 "Iceland") false)

;; Problem 4

;;find-by-business: ListOfBorrower String-> ListOfBorrower
;;Consumes a LoB and a string and produces a LoB if any strings in the business match the business of the borrowers

 (define (find-by-business alob request-business)
   (cond [(empty? alob) empty ]
     	[(cons? alob) (cons (check-business (first alob) request-business)
                        	(find-by-business (rest alob) request-business))]))
 

;;Helper Function
;;check-business: Borrower String -> ListOfBorrower
;;


(define (check-business a-borrower request)
   (if (string=? (borrower-business a-borrower) request)
       (make-borrower (borrower-name a-borrower) (borrower-origin-country a-borrower) 	(borrower-business a-borrower) (borrower-request-loan a-borrower) (borrower-percentage a-borrower))
       empty))
    	

;;test helper function
(check-expect (check-business BORROWER1 "Construction") (make-borrower "Tyler" "United States" "Construction" 12000 .10))
(check-expect (check-business BORROWER2 "Construction") empty)

;;tests for find-by-business
(check-expect (find-by-business BORROWERS "Construction") (cons (make-borrower "Tyler" "United States" "Construction" 12000 .10) (cons empty (cons empty empty))))
(check-expect (find-by-business empty "Construction") empty)


;;Problem 5





