#lang racket

(require test-engine/racket-tests)
(require "hw5.rkt")

; a test case that uses problems 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
(define test1
  (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))))

;-----------------------
; Tests for problem #1.a
;-----------------------
(check-expect (racketlist->mupllist null) (aunit))
(check-expect (racketlist->mupllist (list (var "x"))) (apair (var "x") (aunit)))
(check-expect (racketlist->mupllist (list (var "x") (int 10))) (apair (var "x") (apair (int 10) (aunit))))

;-----------------------
; Tests for problem #1.b
;-----------------------
(check-expect (mupllist->racketlist (aunit)) null)
(check-expect (mupllist->racketlist (apair (var "x") (aunit))) (list (var "x")))
(check-expect (mupllist->racketlist (apair (var "x") (apair (int 10) (aunit)))) (list (var "x") (int 10)))

;-----------------------
; Tests for problem #2
;-----------------------
(check-expect (eval-exp (int 10)) (int 10))
(check-expect (eval-exp (aunit)) (aunit))
(check-expect (eval-exp (closure (fun #f "n" (var "n")) null)) (closure (fun #f "n" (var "n")) null))
(check-expect (eval-exp (isaunit (aunit))) (int 1))
(check-expect (eval-exp (isaunit (int 10))) (int 0))
(check-expect (eval-exp (apair (int 10) (aunit))) (apair (int 10) (aunit)))
(check-expect (eval-exp (apair (isaunit (aunit)) (int 10))) (apair (int 1) (int 10)))
(check-expect (eval-exp (apair (isaunit (int 10)) (int 10))) (apair (int 0) (int 10)))
(check-expect (eval-exp (fst (apair (int 10) (int 20)))) (int 10))
(check-expect (eval-exp (snd (apair (int 10) (int 20)))) (int 20))
(check-expect (eval-exp (apair (fst (apair (int 10) (int 20))) (snd (apair (int 30) (int 40))))) (apair (int 10) (int 40)))
(check-expect (eval-exp (apair (isaunit (fst (apair (aunit) (int 10)))) (int 20))) (apair (int 1) (int 20)))
(check-error (eval-exp (fst (int 10)) "MUPL fst applied to non-pair")) 
(check-error (eval-exp (snd (int 10)) "MUPL fnd applied to non-pair")) 
(check-expect (eval-exp (ifgreater (int 2) (int 1) (int 4) (int 5))) (int 4))
(check-expect (eval-exp (ifgreater (int 1) (int 2) (int 4) (int 5))) (int 5))
(check-expect (eval-exp (ifgreater (int 2) (int 1) (fst (apair (aunit) (int 10))) (snd (apair (int 10) (aunit))))) (aunit))
(check-expect (eval-exp (ifgreater (aunit) (int 1) (fst (apair (aunit) (int 10))) (snd (apair (aunit) (int 10))))) (int 10))
(check-expect (eval-exp (mlet "x" (int 10) (var "x"))) (int 10))
(check-expect (eval-exp (mlet "x" (isaunit (fst (apair (aunit) (int 10)))) (ifgreater (aunit) (int 10) (int 0) (var "x")))) (int 1))
(check-expect (eval-exp (mlet "x" (int 10) (mlet "y" (int 20) (add (var "x") (var "y"))))) (int 30))
(check-expect (eval-exp (fun "map" "xs" (int 10))) (closure null (fun "map" "xs" (int 10))))
(check-error (eval-exp (call (int 10) (aunit))) "MUPL call applied to non-function")
(check-expect (eval-exp (mlet "x" (int 10) (fun "map" "xs" (var "x")))) (closure (list (cons "x" (int 10))) (fun "map" "xs" (var "x"))))
(check-expect (eval-exp (call (fun "plus-one" "n" (add (var "n") (int 1))) (int 10))) (int 11))
(check-expect (eval-exp (call (fun "sum" "n" (ifgreater (var "n") (int 0) (add (var "n") (call (var "sum") (add (var "n") (int -1)))) (int 0))) (int 5))) (int 15))
(check-expect (eval-exp (call (call (fun "plus-x" "n" (fun #f "x" (add (var "n") (var "x")))) (int 5)) (int 4))) (int 9))
(check-expect (eval-exp (call (fun "out" "v" (mlet "x" (int 1) (mlet "f" (fun "in" "n" (add (var "n") (var "x"))) (call (var "f") (int 10))))) (aunit))) (int 11))

;-----------------------
; Tests for problem #3
;-----------------------
(check-expect (eval-exp (ifaunit (aunit) (int 10) (int 20))) (int 10))
(check-expect (eval-exp (ifaunit (int 10) (int 20) (int 30))) (int 30))
(check-expect (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (int 20))) (add (var "x") (var "y")))) (int 30))
(check-expect (eval-exp (mlet* (list (cons "f" (fun #f "n" (add (var "n") (int 1)))) (cons "x" (int 9))) (call (var "f") (var "x")))) (int 10))
(check-expect (eval-exp (ifeq (int 10) (int 10) (int 5) (int 6))) (int 5))
(check-expect (eval-exp (ifeq (int 10) (int 11) (int 5) (int 6))) (int 6))

;-----------------------
; Tests for problem #4
;-----------------------
(check-expect (eval-exp (call (call mupl-map (fun #f "n" (add (var "n") (var "n")))) (apair (int 7) (apair (int 8) (apair (int 9) (aunit))))))
              (apair (int 14) (apair (int 16) (apair (int 18) (aunit)))))
(check-expect (eval-exp (call (call mupl-mapAddN (int 10)) (apair (int 1) (apair (int 2) (apair (int 3) (aunit))))))
              (apair (int 11) (apair (int 12) (apair (int 13) (aunit)))))

;-----------------------
; Tests for challenge
;-----------------------
(check-expect (eval-exp-c (ifaunit (aunit) (int 10) (int 20))) (int 10))
(check-expect (eval-exp-c (ifaunit (int 10) (int 20) (int 30))) (int 30))
(check-expect (eval-exp-c (mlet* (list (cons "x" (int 10)) (cons "y" (int 20))) (add (var "x") (var "y")))) (int 30))
(check-expect (eval-exp-c (mlet* (list (cons "f" (fun #f "n" (add (var "n") (int 1)))) (cons "x" (int 9))) (call (var "f") (var "x")))) (int 10))
(check-expect (eval-exp-c (ifeq (int 10) (int 10) (int 5) (int 6))) (int 5))
(check-expect (eval-exp-c (ifeq (int 10) (int 11) (int 5) (int 6))) (int 6))
(check-expect (eval-exp-c (call (call mupl-map (fun #f "n" (add (var "n") (var "n")))) (apair (int 7) (apair (int 8) (apair (int 9) (aunit))))))
              (apair (int 14) (apair (int 16) (apair (int 18) (aunit)))))
(check-expect (eval-exp-c (call (call mupl-mapAddN (int 10)) (apair (int 1) (apair (int 2) (apair (int 3) (aunit))))))
              (apair (int 11) (apair (int 12) (apair (int 13) (aunit)))))

(test)
