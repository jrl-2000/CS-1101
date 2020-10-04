;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 2-27-20) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct account (acctnum balance))
;; an Account is a (make-account Natural Number)
;; represents a bank account with
;;   acctnum is the account number
;;   balance is the current balance

;; Citibank:  ListOfAccount
;; remembers information about each customer's account
(define Citibank (list (make-account 1 500)
                       (make-account 12 1000)
                       (make-account 3 10)))



;; add-account:  Natural Number -> void
;; consumes an account number and a balance and creates a new account
;; with that information and adds the account to Citibank
;; EFFECT:  when you run function, it will change Citibank
(define (add-account acctnum balance)
  (set! Citibank (cons (make-account acctnum balance) Citibank)))


;; remove-account:  Natural -> void
;; consumes an account number and removes the account with the given account number
;; from the list of accounts
;; EFFECT:  could remove an account from Citibank
(define (remove-account acctnum)
  (set! Citibank (remove-from-list acctnum Citibank)))



;; remove-from-list:  Natural ListOfAccount -> ListOfAccount
;; consumes an account number and a list of accounts and produces a list with the
;; given account number removed
(define (remove-from-list acctnum aloa)
  (cond [(empty? aloa) (error "no such account")]
        [(cons? aloa) (if (= (account-acctnum (first aloa)) acctnum)
                          (rest aloa)
                          (cons (first aloa) (remove-from-list acctnum (rest aloa))))]))

;; testing functions that involve mutation

"show Citibank before running remove-account"
Citibank
"run remove-account to remove account number 12"
(remove-account 12)
"show Citibank no longer contains account 12, but everything else is the same"
Citibank

;; try writing this function on your own (use remove-account as a model)

;; deposit:  Natural Number -> void
;; consumes an account number and an amount to deposit, and adds the amount
;; to the balance of the account with the given account number
;; EFFECT:  could change the balance in one of the accounts in Citibank


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ticketnum:  Natural
;; stores the most recent ticket number dispensed
(define ticketnum 4)

;; get-ticket:  -> Natural
;; produces the next available ticket number
;; EFFECT:  changes the value of ticketnum
(define (get-ticket)
  (begin
    (if (= ticketnum 4)
        (set! ticketnum 1)
        (set! ticketnum (+ 1 ticketnum)))
    ticketnum))
