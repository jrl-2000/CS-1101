;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Lab 4 Lopez|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;Jonathan Lopez, Tyler Sanderville
;;jrlopez, tasanderville
;;jrlopez@wpi.edu
;;tasanderville@wpi.edu

;;group 1072

;;HW 4



;;problem 1

;; make-contact: String String String  -> Contact
(define-struct contact (name address e-mail))

;;a Contact is a (make-contact String String String)
;;interpretation represents a Contact where
;; 	name is the name of the contact
;;      address is the home address of the contact
;;      e-mail is the e-mail address of the contact

;;examples
(define CONTACT1 (make-contact "Jon" "26 Boyton St" "jrlopez@wpi.edu"))
(define CONTACT2 (make-contact "Tyler" "84 Institute Rd" "tasanderville@wpi.edu"))



;; make-charge: String Number  -> Contact
(define-struct charge (business amount-charged))

;;a Charge is a (make-charge String Number)
;;interpretation represents a Charge where
;;   business is the type of business the charge came from
;;   amount-charged is the amount charged to the card


;; a ListOfCharges is one of
;;  empty
;;  (cons String Number ListOfCharges)
;; interp:  ListOfCharges represents a list of Charges

;;examples
(define L1 (list (make-charge "Electronics" 999) (make-charge "Groceries" 89)))
(define L2 (list (make-charge "Electronics" 999) (make-charge "Groceries" 89) (make-charge "Arts" 20)))



;;Problem 2

;;template
;; contact-fcn: Contact ->
;;
#;(define (contact-fcn a-contact)
   (... (contact-name a-contact)
    	(contact-address a-contact)
    	(contact-e-mail a-contact)))

;;template
;; charge-fcn: Charge ->
;;
#;(define (charge-fcn a-charge)
   (... (charge-business a-charge)
    	(charge-amount-charged a-charge)))

;;template
;;loc-fcn: ListOfCharges ->
;;
#;(define (loc-fcn aloc)
  (cond [(empty? aloc) (...) ]
    	[(cons? aloc) (... (charge-fcn (first aloc))
                       	(loc-fcn (rest aloc)))]))
;;Problem 3

;; a BST is one of
;;   false
;;   CustNode

;; a CustNode is a (make-customer Natural Contact Number ListOfCharge BST BST)
(define-struct customer (card-number contact limit charges left right))

;;interp: represents a customers sorted in a tree according to their card number value either left or right.
;;The nodes on the left are sorted in a specifc way and the nodes on the right are sorted in a specifc way was well accroding to their credit card number thier key value.

;;ask about Invarient i didn't get a good explanation

;;Problem 4

    	

    

  