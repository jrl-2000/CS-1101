;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lopez-Sanderville-HW3.bak) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;Jonathan Lopez, Tyler Sanderville
;;jrlopez, tasanderville
;;jrlopez@wpi.edu
;;tasanderville@wpi.edu

;;group 1072

;;HW 3

;;problems 1 and 2 to be done in Lab 3

;;problem 1

; make-borrower: String String String Number Number -> Borrower
(define-struct borrower (name origin-country business request-loan percentage))

;;a Borrower is a (make-borrower String String String Number Number)
;;interpretation represents a Borrower where
;; 	name is the name of the borrower
;;      origin-country is the country if origin of the borrower
;;      business is the business is the kind of business the funds are needed for
;;      request-loan is the funds requested
;;      percentage is the percentage the loan raised so far


;; 3 examples
(define BORROWER1 (make-borrower "Tyler" "United States" "Construction" 12000 .10))
(define BORROWER2 (make-borrower "Jon" "Canada" "Aviation" 1000 .30))
(define BORROWER3 (make-borrower "Joe" "Great Britain" "IT" 5000 .20))



;;template
;; borrower-fcn: Borrower ->
;;

#;(define (borrower-fcn a-borrower)
   (... (borrower-name a-borrower)
    	(borrower-origin-country a-borrower)
        (borrower-business a-borrower)
        (borrower-request-loan a-borrower)
        (borrower-percentage a-borrower)))


;;problem 2




;; a ListOfBorrower is one of
;;  empty
;;  (cons String Number ListOfBorrower)
;; interp:  ListOfBorrower represents a list of Borrowers

;;example
(define BORROWERS (cons BORROWER1 (cons BORROWER2 (cons BORROWER3 empty))))


;;template

;;lob-fcn: ListofBorrower ->
;;

(define (losb-fcn alob)
  (cond [(empty? alob) (...) ]
        [(cons? alob) (... (borrower-fcn (first alob))
                           (lob-fcn (rest alob)))]))



