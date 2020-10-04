;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 2-7-19) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;2/7/19

(define-struct person (name year eye mother father))
;; a Person is a (make-person String Natural String TreeNode TreeNode)
;; interp:  represents a person in a family tree where
;;   name is the person's name
;;   year is year of birth
;;   eye is eye color
;;   mother is the root of the mother's family tree
;;   father is the root of the father's family tree

;; a TreeNode is one of
;;     false
;;     Person (make-person String Natural String TreeNode TreeNode)
;; interp:  represents a family tree, where false means no data in the tree

(define MARYTREE
  (make-person "Mary" 1980 "blue"
               (make-person "Ann" 1960 "green"
                            false
                            (make-person "Fred" 1936 "blue"
                                         false
                                         false))
               (make-person "Joe" 1960 "blue"
                            false
                            false)))

; ;; treenode-fcn:  TreeNode ->
; ;;
; (define (treenode-fcn atn)
;   (cond [(boolean? atn) (...)   ]
;         [(person? atn) (... (person-name atn)
;                             (person-year atn)
;                             (person-eye atn)
;                             (treenode-fcn (person-mother atn))
;                             (treenode-fcn (person-father atn)))]))
; 



;; count-generations:  TreeNode -> Natural
;; consumes a family tree and produces the number of generations represented in the tree
(define (count-generations atn)
  (cond [(boolean? atn) 0 ]
        [(person? atn) (+ 1
                          (max (count-generations (person-mother atn))         ;; Natural
                               (count-generations (person-father atn))))]))     ;; Natural

(check-expect (count-generations false) 0)
(check-expect (count-generations MARYTREE) 3)


;; TRY THIS PROBLEM OVER THE WEEKEND

;; gather-blue-eyed:  TreeNode -> ListOfString
;; consumes a family tree and produces a list of the names of everyone in the
;; tree who has blue eyes

;; gather-blue-eyed:  TreeNode -> ListOfString
;; consumes a family tree and produces a list of names of everyone in the tree who had blue eyes
 (define (gather-blue-eyed atn)
   (cond [(boolean? atn) empty]
         [(person? atn)(if (string=? (person-eye atn) "blue")
                            (cons (person-name atn)
                                  (append (gather-blue-eyed (person-mother atn))
                            (gather-blue-eyed (person-father atn))))
                            (append (gather-blue-eyed (person-mother atn))
                            (gather-blue-eyed (person-father atn))))]))
                                  
                            
                                    
                                    
                                         

;tests
(check-expect (gather-blue-eyed MARYTREE) (list "Mary" "Fred" "Joe"))
(check-expect (gather-blue-eyed false) empty)
(check-expect (gather-blue-eyed (make-person "Mary" 1980 "brown"
               (make-person "Ann" 1960 "green"
                            false
                            (make-person "Fred" 1936 "green"
                                         false
                                         false))
               (make-person "Joe" 1960 "brown"
                            false
                            false))) empty)





;;update-year: TreeNode String Natural -> TreeNode
;;consumes a family tree, the name of the person, and the year that needs to be updated

 (define (update-year atn name mew-year)
   (cond [(boolean? atn) false ]
         [(person? atn) (if (string=?  name (person-name atn))

                            (make-person (person-name atn)
                                         new-year
                                         (person-eye atn)
                                         (person-mother atn)
                                         (person-father atn))
                            (make-person (person-name atn)
                             (person-year atn)
                             (person-eye atn)
                             (update-year (person-mother atn))
                             (update-year (person-father atn))))]))


;;tests
(check-expect (update-year MARYTREE "Fred" 1935)
              
                                         
                                                   




                          
 











