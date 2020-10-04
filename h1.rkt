;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname h1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
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



;; tree-has-child?:  Person -> Boolean
;; consumes a person (representing the root of a descendant family tree)
;; and produces true if anyone in the tree is born after 2002
(define (tree-has-child? aper)
  (or (> (person-year aper) 2002)          
      (any-child-in-list? (person-children aper))))


;; any-child-in-list?:  ListOfPerson -> Boolean
;; consumes a list of persons and produces true if anyone in the list
;; is born after 2002
(define (any-child-in-list? alop)
  (cond [(empty? alop) false]
        [(cons? alop) (or     (tree-has-child? (first alop))         ;; Boolean
                              (any-child-in-list? (rest alop)))]))   ;; Boolean


(check-expect (tree-has-child? SUSANTREE) false)
(check-expect (tree-has-child?
               (make-person "Susan" 1920 "blue"
                            (list (make-person "Joe" 1938 "green" empty)
                                  (make-person "Helen" 1940 "brown"
                                               (list (make-person "Beth" 1965 "green" empty)
                                                     (make-person "Sam" 2004 "brown" empty)))
                                  (make-person "Ricky" 1942 "blue" empty))))
              true)

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
