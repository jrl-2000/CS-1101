;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname hierarchhelp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
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



;; people-with-kids:  Person -> ListOfString
;; consumes a person and produces a list of the names of everyone in
;; the tree who has children
(define (people-with-kids aper)
  (if (empty? (person-children aper))
      empty
      (cons (person-name aper)                            ;; String
            (kids-with-kids (person-children aper)))))    ;; ListOfString


;; kids-with-kids:  ListOfPerson -> ListOfString
;; consumes a list of persons and produces a list of the names of
;; everyone in the list who has children
(define (kids-with-kids alop)
  (cond [(empty? alop) empty]
        [(cons? alop) (append     (people-with-kids (first alop))  ; ListOfString
                                  (kids-with-kids (rest alop)))])) ; ListOfString

(check-expect (people-with-kids SUSANTREE) (list "Susan" "Helen"))


;; The following type of problem (backtracking) will NOT be on Exam 2.
;; However, you will have a problem like this in Homework 5.


;; find-person:  String Person -> Person or false
;; consumes the name of a person and a person, and produces the person
;; in the tree with the given name, or produces false if no such named
;; person in the tree
(define (find-person name aper)
  (if (string=? name (person-name aper))
      aper
      (find-in-list name (person-children aper))))


;; find-in-list:  String ListOfPerson -> Person or false
;; consumes the name of a person and a list of persons and produces the
;; person in the list with the given name, or produces false if no such
;; named person in the list
(define (find-in-list name alop)
  (cond [(empty? alop) false]
        [(cons? alop) (if    (person? (find-person name (first alop)))
                             (find-person name (first alop))
                             (find-in-list name (rest alop)))]))

(check-expect (find-person "Debra" SUSANTREE) false)
(check-expect (find-person "Helen" SUSANTREE) (make-person "Helen" 1940 "brown"
                                                           (list (make-person "Beth" 1965 "green" empty)
                                                                 (make-person "Sam" 1969 "brown" empty))))
(check-expect (find-person "Sam" SUSANTREE) (make-person "Sam" 1969 "brown" empty))