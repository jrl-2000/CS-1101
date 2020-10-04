;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lopez-Sanderville-HW 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;Jonathan Lopez, Tyler Sanderville
;;jrlopez, tasanderville
;;jrlopez@wpi.edu
;;tasanderville@wpi.edu

;;group 1072

;;HW 2

;;problem 1


; make-greeting: String, String -> GreetingCard
(define-struct greeting (message name-of-sender))

;;a GreetingCard is a (make-greeting String String)
;;interpretation represents a greeting card where
;; 	message is the message you want to send in the greeting card
;; 	name-of-sender is the person who is sending the card


;; 1 example
(define GREETING1 (make-greeting "I love you" "Jon"))


;;template
;; greeting-fcn: GreetingCard ->

#;(define (greeting-fcn a-greeting)
   (... (greeting-message a-greeting)
    	(greeting-name-of-sender a-greeting)))


;;problem 2

; make-teddy: String, Natural String GreetingCard -> Teddy
(define-struct teddy (outfit size color a-greeting))



;;a Teddy is a (make-teddy String Natural String GreetingCard)
;;interpretation represents a Teddy Bear where
;; 	Outfit is the type of outfit the bear is wearing
;; 	Size is the sizes the bear comes in either 15 or 20
;; 	Color is the color of the fur
;; 	GreetingCard is the greeting card that comes with the teddy bear



;; 2 examples

(define TEDDY1 (make-teddy "biker" 20 "red" GREETING1))
(define TEDDY2 (make-teddy "tattoo artist" 20 "black" (make-greeting "You're my valentine" "Jon")))

;;template
;; teddy-fcn: Teddy ->

#;(define (teddy-fcn a-teddy)
   (... (teddy-outfit a-teddy)
    	(teddy-size a-teddy)
    	(teddy-color a-teddy)
    	(greeting-fcn (teddy-a-greeting a-teddy))))


;;problem 3

; make-ring: Boolean Boolean GreetingCard -> Ring
(define-struct ring (material heart-box? a-greeting))



;;a Ring is a (make-ring Boolean Boolean GreetingCard)
;;interpretation represents a ring where
;; 	material is what material do you want the ring made of ture is gold; silver=false
;; 	heart-box? is whether you want a heart shaped box with the ring (true)
;; 	GreetingCard is the greeting card that comes with the ring



;; 2 examples

(define RING1 (make-ring true true GREETING1))
(define RING2 (make-ring false true (make-greeting "You're my valentine" "Jon")))

;;template
;; ring-fcn: Ring ->

#;(define (ring-fcn a-ring)
   (... (ring-material a-ring)
    	(ring-heart-box? a-ring)
      	(greeting-fcn (ring-a-greeting a-ring))))


;;problem 4

;;make-roses: Natural, GreetingCard -> roses
(define-struct roses (quantity a-greeting))

;; A rose is a (make-rose natural GreetingCard)
;;interpretation represents a rose where
;;	quantity is the number of dozens of roses in the bundle
;;	GreetingCard is the greeting card that comes with the roses

;; 2 examples

(define ROSE1 (make-roses 5 GREETING1))
(define ROSE2 (make-roses 3 (make-greeting "You're my valentine" "Jon")))

;;template
;;rose-fcn: Rose ->

#;(define (rose-fcn a-rose)
 	(... (ring-quantity a-rose)
      	((greeting-fcn (rose-a-greeting a-rose)))))

;; Problem 5


;; an Gift is one of
;;   GreetingCard
;;   Teddy
;;   Ring
;    Roses

;; data definition for an itemization
;;Gift is a new data type

   
;;make-gift: data-cell-Gift, GreetingCard -> gift
;(define gift (Gift))

;;A Gift is a (make-gift data-cell-Gift GreetingCard
;;interpretation represents a gift where
;;	data-cell-gift is either a ring, roses, or teddy.

;;template
;;gift-fc: Gift ->

#;(define (gift-fcn a-gift)
  (cond [(greeting? a-gift)
         (... (greeting-message a-gift)
              (greeting-name-of-sender a-gift))]
        [(teddy? a-gift)
        	(... (teddy-outfit a-gift)
        	(teddy-size a-gift)
        	(teddy-color a-gift)
        	((greeting-fcn) a-gift))]
    	[(ring? a-gift)
        	(... (ring-material a-gift)
        	(ring-heart-box? a-gift)
        	((greeting-fcn) a-gift))]
    	[(roses? a-gift)
        	(... (roses-quantity a-gift)
        	((greeting-fcn)a-gift))]
    	))


;problem 6


;; greeting-card-cost: GreetingCard -> Number
;;consumes a Greeting Card and produces it's cost based on how many character are in the message portion of the card. 0.05 dollars per charcater.

(define (greeting-card-cost a-greeting)
   (* 0.05 (string-length (greeting-message a-greeting))))

;;tests
(check-expect (greeting-card-cost GREETING1) .5)
(check-expect (greeting-card-cost (make-greeting "You're my valentine" "Jon")) .95)
(check-expect (greeting-card-cost (make-greeting "" "Jon")) 0)



;;problem 7


;;constants
(define size-20 49.99) ;; size 20" bear
(define size-15 39.99) ;; size 15" bear
(define other-fur 5) ;; cost of bear with another color fur

;; teddy-cost: Teddy -> Number
;;consumes a teddy and produces the total cost of a teddy which comes with a Greeting Card
(define (teddy-cost a-teddy)
   (+ (if (= (teddy-size a-teddy) 20)
          size-20
          size-15)
      (cond [(string=? (teddy-color a-teddy) "black") 0]
            [(string=? (teddy-color a-teddy) "brown") 0]
            [else other-fur])
            
    	(greeting-card-cost (teddy-a-greeting a-teddy))))


;;tests
(check-expect (teddy-cost TEDDY1) 55.49) ;a teddy that was red and 20in
(check-expect (teddy-cost TEDDY2) 50.94) ;a teddy that was black and 20in
(check-expect (teddy-cost (make-teddy "biker" 15 "brown" (make-greeting "You're my valentine" "Jon"))) 40.94) ;a teddy that was 15 and brown
(check-expect (teddy-cost (make-teddy "teacher" 15 "orange" (make-greeting "You're my valentine" "Jon"))) 45.94) ; a teddy that was 15 and orange



;;Problem 8

;;constants
(define gold-ring 260) ;; gold ring cost
(define silver-ring 115) ;;silver ring cost
(define heart-box-cost 5.50) ;; heart box cost

;; ring-cost: Ring -> Number
;;consumes a Ring and produces its final cost based on the Material and if a heart shaped box was order. It also includes the cost of the Greeting Card
(define (ring-cost a-ring)
   (+ (cond [(ring-material a-ring) gold-ring]
            [else silver-ring])
         
      (if (ring-heart-box? a-ring)
          heart-box-cost
          0) ;;normal box cost
        
      	(greeting-card-cost (ring-a-greeting a-ring))))

;;(define RING1 (make-ring true true GREETING1))
;;tests

(check-expect (ring-cost RING1) 266) ;; a ring that was gold and had a heart shaped box
(check-expect (ring-cost RING2) 121.45) ;; a ring that was silver and had a heart shaped box
(check-expect (ring-cost (make-ring true false (make-greeting "You're my valentine" "Jon"))) 260.95) ;; a ring that is gold and NOT in a heart shaped box
(check-expect (ring-cost (make-ring false false (make-greeting "You're my valentine" "Jon"))) 115.95) ;; a ring that is silver and NOT in heart shaped box




;;Problem 9

;; Constants
(define price-per-order 24.99)

;;rose-cost: Rose -> Number
;;consumes a rose and produces the final cost of how many bouquees of roes a person orders plus the cost of the Greeting card
(define (rose-cost a-rose)
 	(+ (* (roses-quantity a-rose) price-per-order)
      	(greeting-card-cost (roses-a-greeting a-rose))))


;;tests
(check-expect (rose-cost ROSE1) 125.45) ;; 5 orders of roses with a greeting card
(check-expect (rose-cost ROSE2) 75.92) ;; 3 orders of roses with a different greeting card
(check-expect (rose-cost (make-roses 4 (make-greeting "You're my valentine" "Jon"))) 100.91) ;;making a new data def for roses



;;problem 10

;;constants
(define shipping 10.99)

;;gift-cost: Gift -> Number
;;consumes a Gift and returns the total cost of that gift

(define (gift-cost a-gift)
  (+ (cond [(greeting? a-gift)
         (greeting-card-cost a-gift)]
           
        [(teddy? a-gift)
        (teddy-cost a-gift)]
        
    	[(ring? a-gift)
        (ring-cost a-gift)]
        
    	[(roses? a-gift)
         (rose-cost a-gift)]) shipping))


;;tests with added costs of shipping and handling

(check-expect (gift-cost GREETING1) 11.49)
(check-expect (gift-cost (make-greeting "You're my valentine" "Jon")) 11.94)


(check-expect (gift-cost TEDDY1) 66.48) ;;a teddy that was red and 20in
(check-expect (gift-cost TEDDY2) 61.93) ;;a teddy that was black and 20in
(check-expect (gift-cost (make-teddy "biker" 15 "brown" (make-greeting "You're my valentine" "Jon"))) 51.93) ;;a teddy that was 15 and brown
(check-expect (gift-cost (make-teddy "teacher" 15 "orange" (make-greeting "You're my valentine" "Jon"))) 56.93) ;; a teddy that was 15 and orange

(check-expect (gift-cost RING1) 276.99) ;; a ring that was gold and had a heart shaped box
(check-expect (gift-cost RING2) 132.44) ;; a ring that was silver and had a heart shaped box
(check-expect (gift-cost (make-ring true false (make-greeting "You're my valentine" "Jon"))) 271.94) ;; a ring that is gold and NOT in a heart shaped box
(check-expect (gift-cost (make-ring false false (make-greeting "You're my valentine" "Jon"))) 126.94) ;; a ring that is silver and NOT in heart shaped box

(check-expect (gift-cost ROSE1) 136.44) ;; 5 orders of roses with a greeting card
(check-expect (gift-cost ROSE2) 86.91) ;; 3 orders of roses with a different greeting card
(check-expect (gift-cost (make-roses 4 (make-greeting "You're my valentine" "Jon"))) 111.90) ;;making a new data def for roses

;;Problem 11

;;change-message: Gift String -> Gift
;;consumes a Gift and a String and it updates the Greeting Card Message. It leaves the rest of Gift alone

(define (change-message a-gift message-update)
  (cond [(greeting? a-gift)
         (make-greeting message-update (greeting-name-of-sender a-gift))]
        
        [(teddy? a-gift)
         (make-teddy (teddy-outfit a-gift) (teddy-size a-gift) (teddy-color a-gift) (make-greeting message-update (greeting-name-of-sender (teddy-a-greeting a-gift))))]
         
          
    	[(ring? a-gift)
        	(make-ring (ring-material a-gift) (ring-heart-box? a-gift)  (make-greeting message-update (greeting-name-of-sender (ring-a-greeting a-gift))))]
        
    	[(roses? a-gift)
        	(make-roses (roses-quantity a-gift) (make-greeting message-update (greeting-name-of-sender (roses-a-greeting a-gift))))]
    	))

;;(greeting-name-of-sender a-gift)

;;tests
(check-expect (change-message GREETING1 "Happy Valentines Day!") (make-greeting "Happy Valentines Day!" "Jon")) ;;one for Greeting Card
(check-expect (change-message TEDDY2 "Happy Valentines Day!") (make-teddy "tattoo artist" 20 "black" (make-greeting "Happy Valentines Day!" "Jon"))) ;;one for Teddy
(check-expect (change-message RING1 "Happy Valentines Day!") (make-ring true true (make-greeting "Happy Valentines Day!" "Jon"))) ;; one for Ring
(check-expect (change-message ROSE2 "Happy Valentines Day!") (make-roses 3 (make-greeting "Happy Valentines Day!" "Jon"))) ;; one for Roses




;;Problem 12


;; a ListOfString is one of
;;  empty
;;  (cons String ListOfString)
;; interp:  ListOfString represents a list of strings



;;character-count: ListofString -> Natural
;;consumes a List of Strings and produces all the caracaters there

(define (character-count alos)
  (cond [(empty? alos) 0 ]
        [(cons? alos) (+ (string-length (first alos))
                           (character-count (rest alos)))]))


;;tests
(check-expect (character-count (cons "milk" (cons "tea" (cons "cookies" empty)))) 14)
(check-expect (character-count (cons "Bruins" (cons "Islanders" empty))) 15)
(check-expect (character-count (cons "Sharks" (cons "Kings" empty))) 11)
(check-expect (character-count empty) 0)



;;Problem 13

;;count-X: ListofString -> Natural
;;consumes a list of strings and counts the amount of times the letter x shows up both upper and lower case

;;count-X definition
(define (count-X alos)
  (cond [(empty? alos) 0]
    	[(cons? alos) (+ (help-count-X (explode (first alos)))
                       	(count-X (rest alos)))]))

;; Helper function definition
(define (help-count-X alos)
  (cond [(empty? alos) 0]
    	[(cons? alos) (if (string-ci=? "x" (first alos))
                  	(+ 1 (help-count-X (rest alos)))
                  	(+ 0 (help-count-X (rest alos))))]))

;;tests for count-X

(check-expect (count-X (cons "milk" (cons "xylophone" (cons "Xerox" empty)))) 3)
(check-expect (count-X empty) 0)
(check-expect (count-X (cons "milk" (cons "tea" (cons "cookies" empty)))) 0)

;;tests for helper function

(check-expect (help-count-X empty) 0)
(check-expect (help-count-X (cons "x" empty)) 1)
(check-expect (help-count-X (cons "x"( cons "X" empty))) 2)
(check-expect (help-count-X (cons "o"( cons "X" empty))) 1)
(check-expect (help-count-X (cons "x"( cons "o" empty))) 1)
(check-expect (help-count-X (cons "o"( cons "X" (cons "m" empty)))) 1)


  







              
               
