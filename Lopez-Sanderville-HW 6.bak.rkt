;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Lopez-Sanderville-HW 6.bak|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;;Jonathan Lopez, Tyler Sanderville
;;jrlopez, tasanderville
;;jrlopez@wpi.edu
;;tasanderville@wpi.edu

;;group 1072

;;HW 6



;;PART 1

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


;; a ListOfBorrower is one of
;;  empty
;;  (cons String Number ListOfBorrower)
;; interp:  ListOfBorrower represents a list of Borrowers

;;example
(define BORROWERS (cons BORROWER1 (cons BORROWER2 (cons BORROWER3 empty))))


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

;;help-any-from?: a-borrower, String-> Boolean
;; This functions consumes a borrower and a string and produces true if the
;; origin country of a-borrower

;;tests
(check-expect (any-from? BORROWERS "Canada") true)
(check-expect (any-from? (cons BORROWER2 (cons BORROWER3 empty)) "United States") false)
(check-expect (any-from? empty "Cuba") false)  


;;problem 2

;;find-by-business: ListOfBorrower String-> ListOfBorrower
;;Consumes a LoB and a string and produces a LoB if any strings in the business match the business of the borrowers

(define (find-by-business alob request-business)
  (local [(define (check-business? a-borrower)
            (if (string=? (borrower-business a-borrower) request-business)
                true
                false))]
    (filter check-business? alob)))


;;Helper Function in Local
;;check-business: Borrower String -> Boolean
;; consumes a borrower and a string and produces true if the given string is the same as the borrower's company type.





;;tests for find-by-business
(check-expect (find-by-business BORROWERS "Construction") (cons (make-borrower "Tyler" "United States" "Construction" 12000 .00) empty)) ;(cons empty (cons empty empty))))
(check-expect (find-by-business empty "Construction") empty)
(check-expect (find-by-business (cons BORROWER2 (cons BORROWER1 (cons BORROWER1 empty))) "Construction") (cons (make-borrower "Tyler" "United States" "Construction" 12000 .00)(cons (make-borrower "Tyler" "United States" "Construction" 12000 .00) empty)))


;;Problem 3

;;funds-needed: ListOfBorrowers -> Natural
;; consumes a list of borrowers and produces the total amount of money that
;; these borrowers are still seeking
(define (funds-needed alob)
  (foldr + 0 (map borrower-request-loan alob)))  



;;Tests

(check-expect (funds-needed empty) 0)
(check-expect (funds-needed BORROWERS) (+ 12000 5000 1000))



;;Problem 4

;;no-funds-pledged-helper?: a-borrower-> Boolean
;; consumes a borrower and produces true if the amount pledged is 0


(define (no-funds-pledged alob)
  (pledge-accum alob empty))

(define (pledge-accum alob acc)
  (cond[(empty? alob) acc]
       [(cons? alob) (if (= (borrower-percentage (first alob)) .00)
                         (pledge-accum (rest alob) (cons (borrower-name (first alob)) acc))
                         (pledge-accum (rest alob) acc))]))



;;Tests

(check-expect (no-funds-pledged BORROWERS) (list "Tyler"))
(check-expect (no-funds-pledged (cons BORROWER2 (cons BORROWER1 (cons (make-borrower "Joe" "Great Britain" "IT" 5000 .00) empty)))) (cons "Joe" (cons "Tyler" empty)))
(check-expect (no-funds-pledged empty) empty)



;;PART 2

;;problem 5

(define-struct user (username messages))
;;a user is a (make-user string ListOfMessage)
;;interp: a user in a messaging system where
;;  username is the name of the user
;;  messages is a list of that user's messages

(define-struct message (sender text read?))
;;a message is a (make-message string string boolean)
;;interp: a message is a message in user's inbox where
;;   sender is the username of the sender
;;   text is the actual message body
;;   read? is whether the recipient has read the message


;; a mail system is a list of
;;   empty
;; (cons (user ListOfUser))

(define mailsys empty)

;;problem 6

;;add-user: String -> void
;; consumes a username and adds a new user with the given username to mailsys

(define (add-user username)
  (set! mailsys (cons (make-user username empty) mailsys)))


;;problem 7

;;send-email: string string string-> void
;; consumes a sender username, a recipient username, and a body message and adds the message to the recipients
;; ListOfMessages

(define (send-email sender recipient message)
  (set-user-messages! (find-user recipient mailsys)
              	(cons (make-message sender message false) (user-messages (find-user recipient mailsys)))))
             	 
;;user-finder: String ListOfUser-> a-user
;; consumes a mail system and a username and produces the user with
;; the given name


(define (find-user username alou)
(cond [(empty? alou) empty]
  [(cons? alou) (if (string=? username (user-username (first alou)))
  	(first alou)
  	(find-user username (rest alou)))]))

;;test cases for find-user
(check-expect (find-user "John" (list (make-user "John" empty))) (make-user "John" empty))
(check-expect (find-user "John" empty) empty)
(check-expect (find-user "Tyler" (list (make-user "John" empty) (make-user "Tyler" empty))) (make-user "Tyler" empty))




;;problem 8

;;get-unread-messages-and-mark-read: username -> ListOfMessages
;;consumes a username and returns a ListOfMessages who were previously unread
;;EFFECT: this will change the message data for the user in mailsys

(define (get-unread-messages-and-mark-read username)
  (list (mark-as-read (user-messages (find-user username mailsys)))(user-messages (find-user username mailsys))))

;; mark-as-read: ListofMessage -> ListofMessage
;; consumes a list of unread messages and produces a list of the same
;; ListOfMessages flagged as read

(define (mark-as-read alom)
  (cond
	[(empty? alom) empty]
	[(cons? alom)
 	(if (message-read? (first alom))
     	(mark-as-read (rest alom))
     	(begin
     	(set-message-read?! (first alom) true)
     	(mark-as-read (rest alom))))]))



;;problem 9

;; most-messages: -> User
;; Consumes nothing and produces the user in the mailsystem with the largest number of messages in their mailbox
;; If no users are in the system, the function will return an appropriate error

(define (most-messages)
  (most-messages-accum mailsys 0))

;; accum-most-messages: ListOfUser natural -> a-user
(define (most-messages-accum alou acc)
  (cond [(empty? alou) (error "No such user")]
    	[(cons? alou) (if (> (num-of-mail (user-messages (first alou))) acc)
                      	(first alou)
                      	(most-messages-accum (rest alou) acc))]))


;; how-many-mail-in-list?: ListOfMessages -> Natural
;; consumes a list of masseges in a mail system and produces the number of how much messages is in that list.
(define (num-of-mail alom)
  (cond [(empty? alom) 0]
	[(cons? alom) (+ 1
                  	(num-of-mail (rest alom)))]))


(check-expect (num-of-mail empty) 0)
(check-expect (num-of-mail (list (make-message "John" "yo" true) (make-message "Taylor" "suh" false))) 2)
                                                	 

;;problem 10

;; proof of correct operation
"Show mailsys before running add-user"
mailsys
"Show mailsys after running add-user"
(add-user "John")
mailsys
(add-user "Tyler")
mailsys
""


"Show mailsys before running send-email"
mailsys
"show mailsys after running send-email"
(send-email "John" "Tyler" "The homework is due in an hour!")
(send-email "John" "Tyler" "How'd you do on the exam?")
mailsys
" "


"show mailsys before running fcn"
mailsys
"show mailsys after running fcn"
(get-unread-messages-and-mark-read "Tyler")
mailsys
""

"show output of most-messages"
(most-messages)


