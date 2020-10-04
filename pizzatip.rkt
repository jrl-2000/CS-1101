;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pizzatip) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define (price-per-your-slice slices-eaten total-slices pizza-cost tip-percent)
  (round (* slices-eaten (/ (* pizza-cost (+ tip-percent 1)) total-slices))))

     
     
  
  
  
