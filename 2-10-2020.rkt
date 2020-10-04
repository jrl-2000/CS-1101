;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2-10-2020) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

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




;; gather-blue-eyed:  TreeNode -> ListOfString
;; consumes a family tree and produces a list of the names of everyone in the
;; tree who has blue eyes
(define (gather-blue-eyed atn)
  (cond [(boolean? atn) empty ]
        [(person? atn) (if (string=? "blue" (person-eye atn))


                           (cons (person-name atn)                            ;; String                            
                                 (append (gather-blue-eyed (person-mother atn))      ;; ListOfString
                                         (gather-blue-eyed (person-father atn))))

                           
                           (append (gather-blue-eyed (person-mother atn))      ;; ListOfString
                                   (gather-blue-eyed (person-father atn))))]))


(check-expect (gather-blue-eyed MARYTREE) (list "Mary" "Fred" "Joe"))
(check-expect (gather-blue-eyed false) empty)
(check-expect (gather-blue-eyed
               (make-person "Mary" 1980 "brown"
                            (make-person "Ann" 1960 "green"
                                         false
                                         (make-person "Fred" 1936 "brown"
                                                      false
                                                      false))
                            (make-person "Joe" 1960 "brown"
                                         false
                                         false)))
              empty)


;; update-year:  TreeNode String Natural -> TreeNode
;; consumes a family tree, the name of a person in the tree, and a year of birth;
;; the function produces a family tree the same as the original, except that the
;; year of birth of the named person has been updated to the given year
(define (update-year atn name new-year)
  (cond [(boolean? atn) false  ]
        [(person? atn) (if (string=? name (person-name atn))

                           (make-person (person-name atn)
                                        new-year
                                        (person-eye atn)
                                        (person-mother atn)
                                        (person-father atn))



                           (make-person (person-name atn)
                                        (person-year atn)
                                        (person-eye atn)
                                        (update-year (person-mother atn) name new-year)
                                        (update-year (person-father atn) name new-year)))]))


(check-expect (update-year MARYTREE "Fred" 1935)
              (make-person "Mary" 1980 "blue"
                           (make-person "Ann" 1960 "green"
                                        false
                                        (make-person "Fred" 1935 "blue"
                                                     false
                                                     false))
                           (make-person "Joe" 1960 "blue"
                                        false
                                        false)))

(check-expect (update-year false "Joe" 1958) false)



;; HERE IS THE INVARIANT THAT DEFINES THE BINARY SEARCH TREE PROPERTY:

; 
; A Binary Search Tree is a Binary Tree where, for any node n in the tree,
; the key value of n is > the key values of all nodes in n's left subtree,
; and the key value of n is < the key values of all nodes in n's right
; subtree.
; 
; 