;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname h2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct person (name year eye children))
;; a Person is a (make-person String Natural String ListOfPerson)
;; interp:  represents a person at the root of a descendant family tree
;;   where
;;     name is the person's name
;;     year is their year of birth
;;     eye is their eye color
;;     children is a list of the person's children


;; a ListOfPerson is one of
;;     empty
;;     (cons Person ListOfPerson)


(define SUSANTREE
  (make-person "Susan" 1920 "blue"
               (list (make-person "Joe" 1938 "green" empty)
                     (make-person "Helen" 1940 "brown"
                                  (list (make-person "Beth" 1965 "green" empty)
                                        (make-person "Sam" 1969 "brown" empty)))
                     (make-person "Ricky" 1942 "blue" empty))))

; ;; person-fcn:  Person ->
; ;;
; (define (person-fcn aper)
;   (... (person-name aper)
;        (person-year aper)
;        (person-eye aper)
;        (lop-fcn (person-children aper))))
; 
; 
; ;; lop-fcn:  ListOfPerson ->
; ;;
; (define (lop-fcn alop)
;   (cond [(empty? alop) (...)]
;         [(cons? alop) (...     (person-fcn (first alop))
;                                (lop-fcn (rest alop)))]))
; 



;; Here is where we left the count-older-blue function.  Provide additional test cases, if necessary,
;; and finish the functions on your own.

;; count-older-blue:  Person Natural -> Natural
;; consumes a person and a year of birth and produces the number of people
;; in the tree with blue eyes born before the given year
(define (count-older-blue aper year)
  (... 
   (person-year aper)
   (person-eye aper)
   (lop-fcn (person-children aper))))


;; lop-fcn:  ListOfPerson Natural ->
;;
(define (lop-fcn alop year)
  (cond [(empty? alop) (...)]
        [(cons? alop) (...     (count-older-blue (first alop) year)
                               (lop-fcn (rest alop)))]))


(check-expect (count-older-blue SUSANTREE 1930) 1)
(check-expect (count-older-blue SUSANTREE 1950) 2)

;; list-boomers: Person -> ListOfString
;; consumes a person and produces a list of the names of all people in the
;; tree who were born between 1946 and 1964, inclusive
(define (list-boomers aper)
  (if (and (>= (person-year aper) 1946)
           (<= (person-year aper) 1964))
      

      (cons (person-name aper)                   ;; String       
            (boomers-in-list (person-children aper)))   ;; ListOfString

      (boomers-in-list (person-children aper))))


;; boomers-in-list:  ListOfPerson -> ListOfString
;; consumes a list of persons and produces a list of the names of everyone in
;; the list who is born between 1946 and 1964 (inclusive)
(define (boomers-in-list alop)
  (cond [(empty? alop) empty]
        [(cons? alop) (append     (list-boomers (first alop))        ;; ListOfString
                                  (boomers-in-list (rest alop)))]))  ;; ListOfString

(check-expect (list-boomers SUSANTREE) empty)
(check-expect (list-boomers
               (make-person "Susan" 1920 "blue"
                            (list (make-person "Joe" 1938 "green" empty)
                                  (make-person "Helen" 1947 "brown"
                                               (list (make-person "Beth" 1965 "green" empty)
                                                     (make-person "Sam" 1963 "brown" empty)))
                                  (make-person "Ricky" 1942 "blue" empty))))
              (list "Helen" "Sam"))

               
