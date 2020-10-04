;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |tuesday notes 2-5-20|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct course (dept coursenum term sections seats))
;; a Course is a (make-course String Natural String Natural Natural)
;; interp:  represents a course at WPI
;;  dept is the department
;;   coursenum is the course number
;;   term is the term the course is offered (assume only 1)
;;   sections is the number of sections
;;   seats is the number of seats per section

(define MA2201 (make-course "MA" 2201 "A" 8 30))

; ;; course-template:  Course ->
; ;;
; (define (course-template a-course)
;   (... (course-dept a-course)
;        (course-coursenum a-course)
;        (course-term a-course)
;        (course-sections a-course)
;        (course-seats a-course)))


;; a ListOfCourse is one of
;;   empty
;;   (cons Course ListOfCourse)

(define SCHED (cons MA2201 (cons (make-course "CS" 1101 "C" 6 25) empty)))

; ;; loc-template:  ListOfCourse ->
; ;;
; (define (loc-template aloc)
;   (cond [(empty? aloc) (...)    ]
;         [(cons? aloc) (... (course-template (first aloc))
;                            (loc-template (rest aloc)))]))


;; course-capacity:  ListOfCourse -> Natural
;; consumes a list of courses and produces the total seats in all courses
(define (course-capacity aloc)
  (cond [(empty? aloc) 0]
        [(cons? aloc) (+(capacity-one (first aloc))
                        (course-capacity (rest aloc)))]))

;; capacity-one: Course -> Natural
;; consumes a course and produces the number of seats in all the sections
(define (capacity-one a-course)
  (*(course-sections a-course)
    (course-seats a-course)))

;;test cases
(check-expect (capacity-one MA2201) 240)

(check-expect (course-capacity empty) 0)
(check-expect (course-capacity (cons MA2201 
                                     (cons (make-course "CS" 1101 "C" 6 25) empty))) 390)


;------------------------------------------------------------------------------------
;; any-cs-in-b?:  ListOfCourse -> Boolean
;; consumes a list of courses and determines whether any CS courses are offered in B-term
(define (any-cs-in-b? aloc)
  (cond [(empty? aloc) false]
        [(cons? aloc) (or (cs-in-b? (first aloc))
                          (any-cs-in-b? (rest aloc)))]))


;; cs-in-b?:  Course -> Boolean
;; determines whether the course is a CS course offered in B-term
(define (cs-in-b? a-course)
  (and (string=? "CS" (course-dept a-course))
       (string=? "B"  (course-term a-course))))


;; test cases
(check-expect (cs-in-b? MA2201) false)
(check-expect (cs-in-b? (make-course "CS" 1101 "A" 6 25)) false)
(check-expect (cs-in-b? (make-course "ECE" 3801 "B" 2 30)) false)
(check-expect (cs-in-b? (make-course "CS" 1101 "B" 6 25)) true)

(check-expect (any-cs-in-b? empty) false)
(check-expect (any-cs-in-b? (cons MA2201 
                                  (cons (make-course "CS" 1101 "C" 6 25) empty))) false)
(check-expect (any-cs-in-b? (cons (make-course "BB" 1000 "C" 6 25) 
                                  (cons (make-course "ME" 3801 "D" 4 25) empty))) false)
(check-expect (any-cs-in-b? (cons MA2201 
                                  (cons (make-course "CS" 1101 "B" 6 25) empty))) true)

;------------------------------------------------------------------------------------------

;; multi-sect-course:  ListOfCourse -> ListOfCourse
;; consumes a list of courses and produces list of courses with more than 1 section
(define (multi-sect-course aloc)
  (cond [(empty? aloc) empty]
        [(cons? aloc) 
         (cond [(more-than-1-section? (first aloc)) (cons (first aloc)
                                                          (multi-sect-course (rest aloc)))]
               [else (multi-sect-course (rest aloc))])]))


;; more-than-1-section?:  Course -> Boolean
;; determines whether the course has >1 section
(define (more-than-1-section? a-course)
  (> (course-sections a-course) 1))


;; test cases
(check-expect (more-than-1-section? MA2201) true)
(check-expect (more-than-1-section? (make-course "CS" 2011 "D" 1 50)) false)

(check-expect (multi-sect-course empty) empty)
(check-expect (multi-sect-course (cons (make-course "BB" 1000 "C" 6 25) 
                                       (cons (make-course "ME" 3801 "D" 4 25) empty)))
              (cons (make-course "BB" 1000 "C" 6 25) 
                    (cons (make-course "ME" 3801 "D" 4 25) empty)))
(check-expect (multi-sect-course (cons (make-course "CS" 2011 "D" 1 50)
                                       (cons (make-course "BB" 1000 "C" 6 25) 
                                             (cons (make-course "ME" 3801 "D" 4 25) empty)))) 
              (cons (make-course "BB" 1000 "C" 6 25) 
                    (cons (make-course "ME" 3801 "D" 4 25) empty)))
(check-expect (multi-sect-course (cons (make-course "CS" 1101 "B" 1 30) empty)) empty)

;------------------------------------------------------------------------------------------------------------

;; courses-in-term:  String String ListOfCourse -> ListOfNatural
;; consumes dept name, term, and a list of courses, and returns a list of course numbers offered in that term
;; by that dept

(define (courses-in-term dept term aloc)
  (cond [(empty? aloc) empty]
        [(cons? aloc) 
         (if (course-in-term? dept term (first aloc))
             (cons (course-coursenum (first aloc))
                   (courses-in-term dept term (rest aloc)))
             (courses-in-term dept term (rest aloc)) )]))

;; course-in-term?:  String String Course -> Boolean
;; consumes a course and returns true if that course is given by the dept in the term
(define (course-in-term? dept term a-course)
  (and (string=? (course-dept a-course) dept)
       (string=? (course-term a-course) term)))


;; test cases
(check-expect (course-in-term? "CS" "C" (make-course "MA" 2011 "D" 1 50)) false)
(check-expect (course-in-term? "CS" "C" (make-course "CS" 2011 "D" 1 50)) false)
(check-expect (course-in-term? "CS" "C" (make-course "MA" 2011 "C" 1 50)) false)
(check-expect (course-in-term? "CS" "C" (make-course "CS" 2011 "C" 1 50)) true)

(check-expect (courses-in-term "CS" "C" (cons (make-course "CS" 2011 "D" 1 50)
                                              (cons (make-course "BB" 1000 "C" 6 25) 
                                                    (cons (make-course "ME" 3801 "D" 4 25) empty))))  
              empty)
(check-expect (courses-in-term "CS" "D" (cons (make-course "CS" 2011 "D" 1 50)
                                              (cons (make-course "BB" 1000 "C" 6 25) 
                                                    (cons (make-course "ME" 3801 "D" 4 25) empty))))  
              (cons 2011 empty))
(check-expect (courses-in-term "CS" "D" empty) empty)


;---------------------------------------------------------------------------------------------------

;; add-section: String Natural ListOfCourse -> ListOfCourse
;; consumes dept, course number, and a list of courses and returns a list of courses the same as the original except that
;; the course with the given department and course number now has an additional section

(define (add-section dept coursenum aloc)
  (cond [(empty? aloc) empty]
        [(cons? aloc) 
         (if (course-found? dept coursenum (first aloc))
             (cons (add-section-to-course (first aloc))
                   (add-section dept coursenum (rest aloc)))
             (cons (first aloc)(add-section dept coursenum (rest aloc))))]))

;; course-found? : String Natural Course  -> Boolean
;; consumes a dept and course number, and a course, and returns true if the course has the given dept name and number
(define (course-found? dept coursenum a-course)
  (and (string=? (course-dept a-course) dept)
       (= (course-coursenum a-course) coursenum)))

;; add-section-to-course:  Course -> Course
;; consumes a course and produces a course the same as the original with one more section
(define (add-section-to-course a-course)
  (make-course
   (course-dept a-course)
   (course-coursenum a-course)
   (course-term a-course)
   (+ 1(course-sections a-course))
   (course-seats a-course)))  


;; test cases
(check-expect (course-found? "CS" 1101 (make-course "CS" 2011 "D" 1 50)) false)
(check-expect (course-found? "CS" 1101 (make-course "MA" 1101 "D" 1 50)) false)
(check-expect (course-found? "CS" 1101 (make-course "MA" 2201 "D" 1 50)) false)
(check-expect (course-found? "CS" 1101 (make-course "CS" 1101 "D" 1 50)) true)

(check-expect (add-section-to-course (make-course "CS" 2011 "D" 1 50)) (make-course "CS" 2011 "D" 2 50))

(check-expect (add-section "BB" 1000 (cons (make-course "CS" 2011 "D" 1 50)
                                           (cons (make-course "BB" 1000 "C" 6 25) 
                                                 (cons (make-course "ME" 3801 "D" 4 25) empty)))) 
              (cons (make-course "CS" 2011 "D" 1 50)
                    (cons (make-course "BB" 1000 "C" 7 25) 
                          (cons (make-course "ME" 3801 "D" 4 25) empty))))
(check-expect (add-section "BB" 1000 empty) empty)
(check-expect (add-section "BB" 1000 (cons MA2201 (cons (make-course "CS" 1101 "D" 6 25) empty)))
              (cons MA2201 (cons (make-course "CS" 1101 "D" 6 25) empty)))

;--------------------------------------------------------------------------------------

;; upper-level-courses:  ListOfCourse String -> ListOfCourse
;; consumes a list of courses and a department and returns a list of all
;; courses in the department at the 3000- or 4000 level.


(define (upper-level-courses aloc dept)
  (cond [(empty? aloc) empty]
        [(cons? aloc) 
         (cond [(upper-level? (first aloc) dept) (cons (first aloc)(upper-level-courses (rest aloc) dept))]
               [else (upper-level-courses (rest aloc) dept)])]))


;; upper-level?:  Course String -> Boolean
;; consumes a course and a dept and returns true if the course is in that department and is
;; a 3000- or 4000-leve course
(define (upper-level? a-course dept)
  (and (string=? (course-dept a-course) dept) 
       (and (>= (course-coursenum a-course) 3000) (<= (course-coursenum  a-course) 4999))))


;; test cases
(check-expect (upper-level? (make-course "CS" 2011 "D" 1 50) "CS") false)
(check-expect (upper-level? (make-course "ECE" 2011 "D" 1 50) "CS") false)
(check-expect (upper-level? (make-course "CS" 3043 "D" 1 50) "CS") true)
(check-expect (upper-level? (make-course "ECE" 3801 "D" 1 50) "CS") false)


(check-expect (upper-level-courses (cons (make-course "CS" 2011 "D" 1 50)
                                         (cons (make-course "BB" 1000 "C" 7 25) 
                                               (cons (make-course "ME" 3801 "D" 4 25) empty))) "CS") 
              empty)
(check-expect (upper-level-courses empty "CS") empty)
(check-expect (upper-level-courses (cons (make-course "CS" 2011 "D" 1 50)
                                         (cons (make-course "BB" 1000 "C" 7 25) 
                                               (cons (make-course "ME" 3801 "D" 4 25) empty))) "ME")  
              (cons (make-course "ME" 3801 "D" 4 25) empty))