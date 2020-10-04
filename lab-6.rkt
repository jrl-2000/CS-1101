;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab-6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;Jonathan Lopez, Tyler Sanderville
;;jrlopez, tasanderville
;;jrlopez@wpi.edu
;;tasanderville@wpi.edu

;;group 1072

;;HW 6





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
(define BORROWER1 (make-borrower "Tyler" "United States" "Construction" 12000 .00))
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


;;Problem 1 with higher order function
;;any-from?: ListOfBorrower String-> Boolean
;;Consumes a LoB and a string and produces true if any strings in
;; borrower-origin-country match the input string
(define (any-from? alob country)
  (local [(define (help-any-from? a-borrower)
           (if  (string=? country (borrower-origin-country a-borrower))
    	true
    	false
    	))]
  (ormap help-any-from? alob)))


;;tests
(check-expect (any-from? BORROWERS "Canada") true)
(check-expect (any-from? (cons BORROWER2 (cons BORROWER3 empty)) "United States") false)
(check-expect (any-from? empty "Cuba") false)  

;;problem 2

