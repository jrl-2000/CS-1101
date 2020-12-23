;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Lopez-Sanderville-HW5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;;Jonathan Lopez, Tyler Sanderville
;;jrlopez, tasanderville
;;jrlopez@wpi.edu
;;tasanderville@wpi.edu

;;group 1072

;;HW 5



;;problem 1

;; make-river: String Number Number Natural ListOfRivers -> River
(define-struct river (name ph do flow tributaries))
;;a River is a (make-river String Number Number Natural ListOfRivers)
;;interpretation represents a Contact where
;;     name is the name of the river
;;      ph is the pH of the water
;;      do is the DO (dissolved Oxygen in the river
;;      flow is the flow rate of the river
;;      tributaries is a list of river that feed into this river (tributaries)


;;problem 2

(define MISSOURI-SYSTEM (make-river "Missouri" 7 6.7 10 (list
                                                  (make-river "Jefferson" 5 8.5 18 (list (make-river "Big Hole" 9 5.5 11 empty) (make-river "Beaverhead" 7 8.1 9 empty)))
                                                   (make-river "Yellowstone" 9 7.8 12 (list (make-river "Gardner" 9 6.7 6 empty) (make-river "Shields" 7 7.7 17 empty)
                                                                                            (make-river "Boulder" 10 6.9 11 empty))))))

;;Problem 3

;;template
; 
;  ;; river-fcn:  River ->
  
#;(define (river-fcn ariv)
    (... (river-name ariv)
         (river-ph ariv)
        (river-do ariv)
        (river-flow ariv)
       (lor-fcn (river-tributaries ariv))))

;; lor-fcn:  ListOfRiver ->

#;(define (lor-fcn alor)
    (cond [(empty? alor) (...)]
          [(cons? alor) (...     (river-fcn (first alor))
                                 (lor-fcn (rest alor)))]))
  
 


;; Problem #4
;;count-poor-quality-rivers: River -> Natural
;;Consumes a river and checks to see if it qualifies as a poor river

 (define (count-poor-quality-rivers ariv)
   (if (or (< (river-ph ariv) 6.5) (> (river-ph ariv) 8.5) (< (river-do ariv) 6))
       
       (+ 1 (poor-river (river-tributaries ariv)))
       (poor-river (river-tributaries ariv))))
       
   
    
 
 ;; poor-river:  ListOfRiver -> Natural
 ;;consumes a list of rivers and  counts how many qualify as poor rivers and adds them up
 (define (poor-river alor)
   (cond [(empty? alor) 0 ]
            [(cons? alor) (+ (count-poor-quality-rivers (first alor))
                             (poor-river (rest alor)))]))
;;tests
(check-expect (count-poor-quality-rivers (make-river "Missouri" 7 6.7 10 empty)) 0)
(check-expect (count-poor-quality-rivers MISSOURI-SYSTEM) 5)
(check-expect (count-poor-quality-rivers (make-river "Missouri" 5 6.7 10 (list
                                                  (make-river "Jefferson" 7 8.5 18 (list (make-river "Big Hole" 7 5.5 11 empty) (make-river "Beaverhead" 7 8.1 9 empty)))
                                                   (make-river "Yellowstone" 7 7.8 12 (list (make-river "Gardner" 7 6.7 6 empty) (make-river "Shields" 7 7.7 17 empty)
                                                                                            (make-river "Boulder" 7 6.9 11 empty)))))) 2)

;;tests for helper
(check-expect (poor-river (list (make-river "Missouri" 7 6.7 10 empty))) 0)  
(check-expect (poor-river (list (make-river "Missouri" 5 6.7 10 empty))) 1)
(check-expect (poor-river (list (make-river "Missouri" 5 6.7 10 (list (make-river "Shields" 5 6.7 10 empty))))) 2)  
(check-expect (poor-river empty) 0)  



;;Problem 5
;; low-flow-rivers:  River -> ListOfString
;;consumes amlist of 
 (define (low-flow-rivers ariv flow-r)
   (if (< flow-r (river-flow ariv))
       (cons (river-name ariv)
        (flow-h (river-tributaries ariv) flow-r))
       (flow-h (river-tributaries ariv) flow-r)))
 
 
 ;; flow-h:  ListOfRiver -> ListOFString
 ;;
 (define (flow-h alor flow-r)
   (cond [(empty? alor) empty]
         [(cons? alor) (append (low-flow-rivers (first alor) flow-r)
                                (flow-h (rest alor) flow-r))]))

;;tests
(check-expect (low-flow-rivers  MISSOURI-SYSTEM 10) (list  "Jefferson"  "Big Hole"  "Yellowstone" "Shields" "Boulder"))
(check-expect (low-flow-rivers  MISSOURI-SYSTEM 15) (list  "Jefferson" "Shields"))
(check-expect (low-flow-rivers  MISSOURI-SYSTEM 10) (list  "Jefferson"  "Big Hole"  "Yellowstone" "Shields" "Boulder"))
(check-expect (low-flow-rivers  (make-river "Missouri" 7 6.7 10 empty) 10) empty)

;;test for helper
(check-expect (flow-h (list (make-river "Missouri" 7 6.7 10 empty)) 10) empty)
(check-expect (flow-h (list (make-river "Missouri" 7 6.7 10 empty)) 9) (list "Missouri"))
(check-expect (flow-h (list (make-river "Missouri" 5 6.7 10 (list (make-river "Shields" 5 6.7 10 empty)))) 9) (list "Missouri" "Shields"))




 


;;Problem 6
;;lower-all-ph: River System -> River System
;; Consumes a River System and produces the same River system but with each river's ph lowered by 0.1 

(define (lower-all-ph ariv)
 (make-river (river-name ariv)
         (- (river-ph ariv) 0.1)
         (river-do ariv)
      (river-flow ariv)
  (lower-all-ph-helper (river-tributaries ariv))))
      
                           



;;lower-all-ph-helper: River -> River
;;consumes a ListOfRiver and runs each river through the lower-all-ph fcn.

(define (lower-all-ph-helper alor)
  (cond [(empty? alor) empty]
  [(cons? alor) 
         (cons (lower-all-ph (first alor))
         (lower-all-ph-helper (rest alor)))]))
                

;;tests
(check-expect (lower-all-ph MISSOURI-SYSTEM) (make-river "Missouri" 6.9 6.7 10 (list
      (make-river "Jefferson" 4.9 8.5 18 (list (make-river "Big Hole" 8.9 5.5 11 empty) (make-river "Beaverhead" 6.9 8.1 9 empty)))
      (make-river "Yellowstone" 8.9 7.8 12 (list (make-river "Gardner" 8.9 6.7 6 empty) (make-river "Shields" 6.9 7.7 17 empty)
                                                   (make-river "Boulder" 9.9 6.9 11 empty))))))

(check-expect (lower-all-ph (make-river "Missouri" 7 6.7 10 empty)) (make-river "Missouri" 6.9 6.7 10 empty))

(check-expect (lower-all-ph (make-river "Missouri" 5 6.7 10 (list
                                                  (make-river "Jefferson" 7 8.5 18 (list (make-river "Big Hole" 7 5.5 11 empty) (make-river "Beaverhead" 7 8.1 9 empty)))
                                                   (make-river "Yellowstone" 7 7.8 12 (list (make-river "Gardner" 7 6.7 6 empty) (make-river "Shields" 7 7.7 17 empty)
                                                                                            (make-river "Boulder" 7 6.9 11 empty))))))

              (make-river "Missouri" 4.9 6.7 10 (list
                                                  (make-river "Jefferson" 6.9 8.5 18 (list (make-river "Big Hole" 6.9 5.5 11 empty) (make-river "Beaverhead" 6.9 8.1 9 empty)))
                                                   (make-river "Yellowstone" 6.9 7.8 12 (list (make-river "Gardner" 6.9 6.7 6 empty) (make-river "Shields" 6.9 7.7 17 empty)
                                                                                            (make-river "Boulder" 6.9 6.9 11 empty))))))


;; tests helper fcn


(check-expect (lower-all-ph-helper (list
      (make-river "Jefferson" 4.9 8.5 18 (list (make-river "Big Hole" 8.9 5.5 11 empty) (make-river "Beaverhead" 6.9 8.1 9 empty)))
      (make-river "Yellowstone" 8.9 7.8 12 (list (make-river "Gardner" 8.9 6.7 6 empty) (make-river "Shields" 6.9 7.7 17 empty)
                                                   (make-river "Boulder" 9.9 6.9 11 empty)))))

              (list
      (make-river "Jefferson" 4.8 8.5 18 (list (make-river "Big Hole" 8.8 5.5 11 empty) (make-river "Beaverhead" 6.8 8.1 9 empty)))
      (make-river "Yellowstone" 8.8 7.8 12 (list (make-river "Gardner" 8.8 6.7 6 empty) (make-river "Shields" 6.8 7.7 17 empty)
                                                   (make-river "Boulder" 9.8 6.9 11 empty)))))

(check-expect (lower-all-ph-helper empty) empty)

;;Problem 7

;;find-subsytem: String River-system -> River-system / Boolean
;;Consumes a name of a river and a river system and returns the river data if the
;; river is in the system while returning false if the river is absent.


(define (find-subsystem ariv name)
    (if (string=? name (river-name ariv))
         ariv
       (find-subsystem-helper (river-tributaries ariv) name)))

;;find-subsystem-helper: ListOfRiver natural -> boolean / a-river

(define (find-subsystem-helper alor name)
    (cond [(empty? alor) false]
          [(cons? alor) (if (river? (find-subsystem (first alor) name))
                                     (find-subsystem (first alor) name)
                                     (find-subsystem-helper (rest alor) name))]))
                      


;;tests

(check-expect (find-subsystem MISSOURI-SYSTEM "Jefferson") (make-river "Jefferson" 5 8.5 18 (list (make-river "Big Hole" 9 5.5 11 empty) (make-river "Beaverhead" 7 8.1 9 empty))))
(check-expect (find-subsystem MISSOURI-SYSTEM "Beaverhead") (make-river "Beaverhead" 7 8.1 9 empty))
(check-expect (find-subsystem MISSOURI-SYSTEM "Nile") false)
(check-expect (find-subsystem MISSOURI-SYSTEM "Boulder")  (make-river "Boulder" 10 6.9 11 empty))

;;helper-tests

(check-expect (find-subsystem-helper  (list
                                                  (make-river "Jefferson" 5 8.5 18 (list (make-river "Big Hole" 9 5.5 11 empty) (make-river "Beaverhead" 7 8.1 9 empty)))
                                                   (make-river "Yellowstone" 9 7.8 12 (list (make-river "Gardner" 9 6.7 6 empty) (make-river "Shields" 7 7.7 17 empty)
                                                                                            (make-river "Boulder" 10 6.9 11 empty))))
 "Jefferson") (make-river "Jefferson" 5 8.5 18 (list (make-river "Big Hole" 9 5.5 11 empty) (make-river "Beaverhead" 7 8.1 9 empty))))
(check-expect (find-subsystem-helper empty "Nile") false)
(check-expect (find-subsystem-helper (list
                                                  (make-river "Jefferson" 5 8.5 18 (list (make-river "Big Hole" 9 5.5 11 empty) (make-river "Beaverhead" 7 8.1 9 empty)))
                                                   (make-river "Yellowstone" 9 7.8 12 (list (make-river "Gardner" 9 6.7 6 empty) (make-river "Shields" 7 7.7 17 empty)
                                                                                           (make-river "Boulder" 10 6.9 11 empty))))
  "Boulder") (make-river "Boulder" 10 6.9 11 empty))





;;Problem 8

;;count-alkaline-tributaries-of: String river-system -> Natural
;;consumes a name of river and a river system and produces a count of the
;; named river's tributaries that have a pH of over 8.5

(define (count-alkaline-tributaries-of ariv name)
     (count-alkaline-tributaries-of-recursive-helper (find-subsystem ariv name)))

;;count-alkaline-tributaries-of-recursive-helper: a-river -> Natural
;; consumes a river from either the count-alkaline-tributaries-of fcn
;; or the add-if-alkaline fcn and produces the output of add-if-alkaline
;; once recursion is cycled through

(define (count-alkaline-tributaries-of-recursive-helper ariv)
               (add-if-alkaline (river-tributaries ariv)))

;;add-if-alkaline: ListOfRiver -> Natural
;;consumes the list of tributaries from count-alkaline-tributaries-of-recursive-helper
;; and adds 1 for every tributary with a pH over 8.5

(define (add-if-alkaline alor)
    (cond [(empty? alor) 0]
          [(cons? alor) (if (< 8.5 (river-ph (first alor)))
                            (+ 1 (count-alkaline-tributaries-of-recursive-helper (first alor)) (add-if-alkaline (rest alor)))
                                 (+ (count-alkaline-tributaries-of-recursive-helper (first alor)) (add-if-alkaline (rest alor))))]))




           
;;tests
(check-expect (count-alkaline-tributaries-of MISSOURI-SYSTEM "Jefferson") 1)
(check-expect (count-alkaline-tributaries-of MISSOURI-SYSTEM "Yellowstone") 2)
(check-expect (count-alkaline-tributaries-of MISSOURI-SYSTEM "Gardner") 0)
(check-expect (count-alkaline-tributaries-of MISSOURI-SYSTEM "Missouri") 4)
(check-expect (count-alkaline-tributaries-of (make-river "Missouri" 5 6.7 10 (list (make-river "Shields" 5 6.7 10 empty))) "Missouri") 0)
    
;tests for count-alkaline-tributaries-of-recursive-helper
(check-expect (count-alkaline-tributaries-of-recursive-helper (make-river "Missouri" 7 6.7 10 empty)) 0)
(check-expect (count-alkaline-tributaries-of-recursive-helper (make-river "Shields" 9 6.7 10 empty)) 0)
(check-expect (count-alkaline-tributaries-of-recursive-helper (make-river "Jefferson" 5 8.5 18 (list (make-river "Big Hole" 9 5.5 11 empty) (make-river "Beaverhead" 7 8.1 9 empty))))
              1)

;;tests for add-if-alkaline
(check-expect (add-if-alkaline (list (make-river "Missouri" 7 6.7 10 (list
                                                                      (make-river "Jefferson" 5 8.5 18 (list (make-river "Big Hole" 9 5.5 11 empty) (make-river "Beaverhead" 7 8.1 9 empty)))
                                                                      (make-river "Yellowstone" 9 7.8 12 (list (make-river "Gardner" 9 6.7 6 empty) (make-river "Shields" 7 7.7 17 empty)
                                                                                                               (make-river "Boulder" 10 6.9 11 empty))))))) 4)

(check-expect (add-if-alkaline (list 
                                (make-river "Jefferson" 5 8.5 18 (list (make-river "Big Hole" 9 5.5 11 empty) (make-river "Beaverhead" 7 8.1 9 empty)))
                                
                                )) 1)

















