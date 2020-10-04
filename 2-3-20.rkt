;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2-3-20) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct course (dept coursenum term sections seats))
;; a Course is a (make-course String Natural String Natural Natural)
;; interp:  a course at WPI, where
;;   dept is the department that offers the course
;;   coursenum is the course number
;;   term is the term the course is offered (assume only 1)
;;   sections is the number of sections of the course
;;   seats is the number of seats per section

(define MA2201 (make-course "MA" 2201 "A" 8 30))

; ;; course-fcn:  Course ->
; ;;
; (define (course-fcn a-course)
;   (... (course-dept a-course)
;        (course-coursenum a-course)
;        (course-term a-course)
;        (course-sections a-course)
;        (course-seats a-course)))
; 
; 


;; a ListOfCourse is one
;;   empty
;;   (cons Course ListOfCourse)

(define SCHED (cons (make-course "CS" 2102 "B" 6 25) (cons MA2201 empty)))

; ;; loc-fcn:  ListOfCourse ->
; ;;
; (define (loc-fcn aloc)
;   (cond [(empty? aloc) (...)    ]
;         [(cons? aloc)   (... (course-fcn (first aloc))
;                              (loc-fcn (rest aloc)))]))
; 


;; course-capacity:  ListOfCourse -> Natural
;; consumes a list of courses and produces the total number of seats in all
;; sections in all courses in the list
(define (course-capacity aloc)
  (cond [(empty? aloc) 0   ]
        [(cons? aloc)   (+ (capacity-of-one (first aloc))
                           (course-capacity (rest aloc)))]))

(check-expect (course-capacity SCHED) 390)
(check-expect (course-capacity empty) 0)

;; capacity-of-one:  Course -> Natural
;; consumes a course and produces the number of seats in all sections of the course
(define (capacity-of-one a-course)
  (* 
   (course-sections a-course)
   (course-seats a-course)))

(check-expect (capacity-of-one (make-course "CS" 1101 "C" 6 25)) 150)


;; TRY THIS PROBLEM TONIGHT

;; any-cs-in-b?:  ListOfCourse -> Boolean
;; consumes a list of courses and produces true if any of the courses in the
;; list are CS courses offered in B-term


;;HW

(define (any-cs-in-b? aloc)
  (cond [(empty? aloc) false]
        [(cons? aloc) (or (cs? (first aloc))
                         (any-cs-in-b? (rest aloc)))]))
  
(define (cs? a-course)
  (and (string=? "CS" (course-dept a-course)) (string=? "B" (course-term a-course))
       
       ))

(check-expect (any-cs-in-b? empty) false)
(check-expect (any-cs-in-b? SCHED) true)

;;multi-section-course: ListOfCourse -> ListOfCourse
;; consumes a list of courses and produces a list if courses with more then 1 section
  (define (multi-section-course aloc)
   (cond [(empty? aloc) empty ]
         [(cons? aloc) (or (multi? (first aloc))
                             (multi-section-course (rest aloc)))
                             ]))




;;multi?: ListOfCourse -> ListOfCourse
;; consumes a courses and produces a course if the courses has more than 1 section


 (define (multi? a-course)
   (if  (> (course-sections a-course) 1)
        (make-course (course-dept a-course) (course-coursenum a-course) (course-term a-course) (course-sections a-course) (course-seats a-course)) 
        empty)
)
 
(check-expect (multi-section-course SCHED) (cons (make-course "CS" 2102 "B" 6 25) (cons (make-course "MA" 2201 "A" 8 30)  empty))) 
(check-expect (multi-section-course empty) empty)


