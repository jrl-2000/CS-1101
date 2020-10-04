;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |how does recursion work|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; How does recursion work?  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here's a function that uses recursion to sum up the numbers in a list

;; sum:  ListOfNumber -> Number
;; consumes a list of numbers and produces the sum of the numbers in the list
(define (sum alon)
  (cond [(empty? alon) 0]
        [(cons? alon) (+ (first alon) (sum (rest alon)))]))

;; SHOW EVERY STEP IN THE EVALUATION OF THIS EXPRESSION:
(sum (cons 3 (cons 9 empty)))


;; HERE ARE THE FIRST FEW STEPS
(cond [(empty? (cons 3 (cons 9 empty))) 0]
      [(cons? (cons 3 (cons 9 empty))) (+ (first (cons 3 (cons 9 empty)))
                                          (sum (rest (cons 3 (cons 9 empty)))))])

(cond [false 0]
      [(cons? (cons 3 (cons 9 empty))) (+ (first (cons 3 (cons 9 empty)))
                                          (sum (rest (cons 3 (cons 9 empty)))))])

(cond [(cons? (cons 3 (cons 9 empty))) (+ (first (cons 3 (cons 9 empty)))
                                          (sum (rest (cons 3 (cons 9 empty)))))])

(cond [true (+ (first (cons 3 (cons 9 empty)))
               (sum (rest (cons 3 (cons 9 empty)))))])


(+ (first (cons 3 (cons 9 empty)))
   (sum (rest (cons 3 (cons 9 empty)))))

(+ 3
   (sum (rest (cons 3 (cons 9 empty)))))

;; YOU SHOULD WRITE OUT THE REMAINING STEPS (there are 13 more steps)

;; (DON'T LOOK AT THE ANSWERS BELOW UNTIL YOU TRY IT YOURSELF!)

;; (SCROLL DOWN TO SEE THE ANSWERS)





































;; HERE ARE THE REMAINING STEPS

(+ 3                         
   (sum (cons 9 empty)))

(+ 3
   (cond [(empty? (cons 9 empty)) 0]
         [(cons? (cons 9 empty)) (+ (first (cons 9 empty))
                                    (sum (rest (cons 9 empty))))]))

(+ 3 (cond [false 0]
           [(cons? (cons 9 empty)) (+ (first (cons 9 empty))
                                      (sum (rest (cons 9 empty))))]))

(+ 3 (cond  [(cons? (cons 9 empty)) (+ (first (cons 9 empty))
                                       (sum (rest (cons 9 empty))))]))

(+ 3 (cond [true (+ (first (cons 9 empty))
                    (sum (rest (cons 9 empty))))]))

(+ 3 (+ (first (cons 9 empty))
        (sum (rest (cons 9 empty)))))

(+ 3 (+ 9 (sum (rest (cons 9 empty)))))

(+ 3 (+ 9 (sum empty)))

(+ 3 (+ 9 (cond [(empty? empty) 0]
                [(cons? empty) (+ (first empty)
                                  (sum (rest empty)))])))

(+ 3 (+ 9 (cond [true 0]
                [(cons? empty) (+ (first empty)
                                  (sum (rest empty)))])))

(+ 3 (+ 9 0))

(+ 3 9)

12



           