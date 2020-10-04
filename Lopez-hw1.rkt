;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lopez-hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;Jonathan Lopez
;;jrlopez
;;jrlopez@wpi.edu
;;problem 1

;; make-vehicle: String, String, Natural, Natural, Natural, Date -> vehicle
(define-struct vehicle (Make Model Year-of-make Odometer Max-occupants Date))

;; make-date: Natural, Natural, Natural -> date
(define-struct date (Year Month Day))


(define date1 (make-date 2020 12 24))
(define date2 (make-date 2022 6 19))
(define date3 (make-date 2020 2 26))


(define vehicle1 (make-vehicle "Toyota" "86" 2017 56090 4 date1))
(define vehicle2 (make-vehicle "Saab" "9-3" 2011 98657 5 date2))
(define vehicle3 (make-vehicle "Honda" "S2000" 2009 134209 2 date3))

;;Problem #2

;;signature for vehicle

;;make-vehicle: String, String, Natural, Natural, Natural, Date -> vehicle

;;vehicle-Make: Vehicle -> String
;;vehicle-Model: Vehicle -> String
;;vehicle-Year-of-make: Vehicle -> Natural
;;vehicle-Odometer: Vehicle -> Natural
;;vehicle-Max-occupants: -> Natural
;;vehicle-Date: Vehicle -> date


;;Problem 3

(define (low-mileage-toyota? vehicle Odometer)
 (if  
  




