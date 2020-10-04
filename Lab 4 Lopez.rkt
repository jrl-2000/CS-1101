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


;;interp. false mean there's no BST or empty BST
;;   card number is the number on the card
;;   contact is a Contact
;;   limit is the amount of money limited to them
;;   charges is a Charges
;;   left is a BST
;;   right is a BST


;;INV: Left and right are subtrees where the number of the  their credit card number thier key value numerical order
;;The left subtree has number less than the original node and on ther rright has umber greater than the node


;;Problem 4

(define customer1 (make-customer 1001 CONTACT1 185.8 L1 false false))
(define customer2 (make-customer 5029 CONTACT2 203.9 L2 false false))
(define customer3 (make-customer 2049 (make-contact "Nate" "13 Smith St" "nate01@gmail.com") 403.4
                        (list (make-charge "Software" 199) (make-charge "Gaming" 849) (make-charge "Theater" 20))  customer1 false))
(define customer4 (make-customer 4049 (make-contact "Zack" "26 Smith St" "zack01@gmail.com") 608.7
                        (list (make-charge "Software" 269) (make-charge "Video Games" 549) (make-charge "Groceries" 20))  false customer2))
(define customer5 (make-customer 3011 (make-contact "Billy" "29 Smith St" "billy01@gmail.com") 909.7
                        (list (make-charge "Gaming" 849) (make-charge "Theater" 20))  customer3 customer4))

;;problem 5

; ;; treenode-fcn:  TreeNode ->
; ;;
; (define (treenode-fcn atn)
;   (cond [(boolean? atn) (...)   ]
;         [(customer? atn) (... (customer-card-number atn)
;                             (customer-contact atn)
;                             (customer-limit atn)
;                             (customer-charges atn)
;                             (treenode-fcn (customer-left atn))
;                             (treenode-fcn (customer-right atn)))]))

;;problem 6

;; any-with-greater-limit?:  TreeNode -> Boolean
;; 
 (define (any-with-greater-limit? atn alimit)
   (cond [(false? atn)  false   ]
         [(customer? atn) (if (< alimit (customer-limit atn))
                              true
                              (or
                             
                             (any-with-greater-limit? (customer-left atn) alimit)
                             (any-with-greater-limit? (customer-right atn) alimit)))]))

;;tests
(check-expect (any-with-greater-limit? customer5 100) true)
(check-expect (any-with-greater-limit? customer1 200) false)
(check-expect (any-with-greater-limit? customer1 100) true)
(check-expect (any-with-greater-limit? customer2 200) true)
(check-expect (any-with-greater-limit? customer3 200) true)
(check-expect (any-with-greater-limit? customer4 2000) false)



;;problem 7

;; total-charge-for:  TreeNode -> Number
;; consumes a BST and a credit card number and produces the charge amount on that customer's specific credit card
 (define (total-charge-for atn ccn)
   (cond [(false? atn) -1]
         [(customer? atn)
          (cond  [(= ccn (customer-card-number atn))
                  (charge-amount (customer-charges atn))]
                 [(< ccn (customer-card-number atn))
                     (total-charge-for (customer-left atn) ccn)]
                 [(> ccn (customer-card-number atn))
                  (total-charge-for (customer-right atn) ccn)])]))


;;tests 
(check-expect (total-charge-for customer1 1001) 1088)
(check-expect (total-charge-for customer5 1501) -1)
(check-expect (total-charge-for customer3 1001) 1088)
(check-expect (total-charge-for customer5 1001) 1088)
(check-expect (total-charge-for customer5 5029) 1108)



;;charge-amount: ListOfCharges -> Number
;; takes in a list of charges and produces the amount of the charges

(define (charge-amount aloc)
  (cond [(empty? aloc) 0 ]
    	   [(cons? aloc) (+ (charge-amount-charged (first aloc))
                       	(charge-amount (rest aloc)))]))
  

               
;;Problem 8
;;customers-with-outstanding-charges: TreeNode -> ListOfString
;; consumes a BST and produces a list of all customer's email addresses with outstanding credit card charges

(define (customers-with-outstanding-charges atn)
   (cond [(false? atn) empty]
         [(customer? atn) (if (< 0 (charge-amount (customer-charges atn)))
                              (cons (contact-e-mail (customer-contact atn))
                                    (append (customers-with-outstanding-charges (customer-left atn))
                                            (customers-with-outstanding-charges (customer-right atn))))
                              (append (customers-with-outstanding-charges (customer-left atn))
                                            (customers-with-outstanding-charges (customer-right atn))))]))

 


                        
;;tests
(check-expect (customers-with-outstanding-charges customer1) (list "jrlopez@wpi.edu"))
(check-expect (customers-with-outstanding-charges customer5) (list "billy01@gmail.com" "nate01@gmail.com" "jrlopez@wpi.edu"  "zack01@gmail.com" "tasanderville@wpi.edu" ))
(check-expect (customers-with-outstanding-charges customer2) (list "tasanderville@wpi.edu"))
(check-expect (customers-with-outstanding-charges customer3) (list "nate01@gmail.com" "jrlopez@wpi.edu"))
(check-expect (customers-with-outstanding-charges customer4) (list"zack01@gmail.com" "tasanderville@wpi.edu"))
(check-expect (customers-with-outstanding-charges false) empty)






                              
                             






























    

  