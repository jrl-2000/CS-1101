;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;pens costs $.75 or fewer are ordered, and they cost $.50 if more than 10 are ordered

;;single-pen-cost: Number -> Number
;;consumes the number of pens in an order and produces the cost of a single pen as described above

(define (single-pen-cost num-pens)
  (if (<= num-pens 10)
      0.75
      0.50))

;;test

(check-expect (single-pen-cost 13) 0.50)
(check-expect (single-pen-cost 10) 0.75)
(check-expect (single-pen-cost 9) 0.75)

;;printed pen cost: Number String-> Number
;;consumes the number of pens in an order and a slogan to print  on each pen, and produces the total the total cost of the order, where the base cost if each pen is above,
;;slogan cost 2 cents per letter per pen and a printing surcharge of $3 is applied to the entire order
(define (printed-pen-cost numpens slogan)
  (+
   (*
    (+(single-pen-cost numpens)
    (* 0.02 (string-length slogan)))
    numpens)
    3))
           

  
;;test
(check-expect (printed-pen-cost 1 "WPI") 3.81)
(check-expect (printed-pen-cost 20 "CS1101") 15.40)
