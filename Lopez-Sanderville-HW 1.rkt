;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lopez-Sanderville-HW 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;Jonathan Lopez, Tyler Sanderville
;;jrlopez, tasanderville
;;jrlopez@wpi.edu
;;tasanderville@wpi.edu

;;group 1072

;;HW 1

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
(define vehicle3 (make-vehicle "Honda" "S2000" 2011 134209 2 date3))


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

;;low-mileage-toyota?: Vehicle Natural -> Boolean
;;consumes a vehicle and a requested mileage and produces true if there's a Toyota with less than the requested mileage

(define (low-mileage-toyota? a-vehicle mileage-request)
 (and (string=? "Toyota"  (vehicle-Make a-vehicle)) (> mileage-request (vehicle-Odometer a-vehicle))))



;tests for low-mileage-toyota?
(check-expect (low-mileage-toyota? vehicle1 60000) true) ;; true a toyota with less then the requested mileage
(check-expect (low-mileage-toyota? vehicle1 50000) false) ;; toyota with more than the requesterd mileage
(check-expect (low-mileage-toyota? vehicle2 60000) false) ;; not a toyota with less the mileage requested
(check-expect (low-mileage-toyota? vehicle2 50000) false) ;; not a toyota with more the mileage requested


;problem 4
;;newer-car: Vehicle, Vehicle -> Vehicle (dataset)
;;consumes two vehicles and produces the vehicle that has the most recent year of manufactureif both are the same it returns the first vehicle

(define (newer-car vehicle a-vehicle)
(cond [(< (vehicle-Year-of-make a-vehicle) (vehicle-Year-of-make vehicle)) vehicle]
      [(> (vehicle-Year-of-make a-vehicle) (vehicle-Year-of-make vehicle)) a-vehicle]
      [else vehicle]
  ))

;;tests  
(check-expect (newer-car vehicle1 vehicle2) vehicle1)
(check-expect (newer-car vehicle2 vehicle1) vehicle1)
(check-expect (newer-car vehicle3 vehicle2) vehicle3)


;problem 5
;;update-odometer: Natural -> Vehicle (dataset)
;;consumes a vehicle  and the number of miles a rental car has driven  and updates the car's odometer with the new reading produces a vehicle

(define (update-odometer vehicle numOfMiles)
  (make-vehicle
   (vehicle-Make vehicle)
   (vehicle-Model vehicle)
   (vehicle-Year-of-make vehicle)
 (+ (vehicle-Odometer vehicle) numOfMiles)
 (vehicle-Max-occupants vehicle)
 (vehicle-Date vehicle)))


(check-expect (update-odometer vehicle1 125) (make-vehicle "Toyota" "86" 2017 (+ 56090 125) 4 date1))
(check-expect (update-odometer vehicle2 3000)(make-vehicle "Saab" "9-3" 2011 (+ 98657 3000) 5 date2))
(check-expect (update-odometer vehicle3 50000)(make-vehicle "Honda" "S2000" 2011 (+ 134209 50000) 2 date3))

;;problem 6

;;registration-valid? Vehicle Date -> Boolean
;;compares the vehicle's expiration date with a given date and produces a boolean whether it's valid or not

(define (registration-valid? a-vehicle date)
  (cond [(> (date-Year (vehicle-Date a-vehicle)) (date-Year date)) false]
        [(> (date-Month (vehicle-Date a-vehicle)) (date-Month date)) false]
        [(> (date-Day (vehicle-Date a-vehicle)) (date-Day date)) false]
        (else true)))
;tests
(check-expect (registration-valid? vehicle1 (make-date 2020 12 25)) true)
(check-expect (registration-valid? vehicle1 (make-date 2020 12 24)) true)
(check-expect (registration-valid? vehicle2 (make-date 2020 12 23)) false)


 





