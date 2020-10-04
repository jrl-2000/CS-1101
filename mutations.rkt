;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname mutations) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
(define-struct account (acctnum balance))
;;an Account is a (make-account Natural Number)
;;reps a bank account where
;;     acctnum is the bank accounts number
;;     balance is the current balance

;; a ListOfAccounts is one of
;;    empty
;;    (cons Account ListOfAccount)



;;Citibank: ListOfAccount
;;remebers information about each customer's accounts
(define Citibank (list (make-account 1 500)
                       (make-account 12 1000)
                       (make-account 3 10)))

(define-struct customer (name phone acct))
;;a Customer is a (make-customer String natural Account
;;interp
;;create data for two people who share an account
(define MPacct (make-account 24 500))
(define Mucst (make-customer "Maria" 5551234 MPacct))
(define pucst (make-customer "Phill" 5556789 MPacct))


