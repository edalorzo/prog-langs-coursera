;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;-----------------------
; Problem #1.a
;-----------------------
(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

;-----------------------
; Problem #1.b
;-----------------------
(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(isaunit? e) (let ([v (eval-under-env (isaunit-e e) env)])
                        (if (aunit? v) (int 1) (int 0)))]
        [(apair? e) (let ([l (eval-under-env (apair-e1 e) env)]
                          [r (eval-under-env (apair-e2 e) env)])
                      (apair l r))]
        [(fst? e) (let ([v (eval-under-env (fst-e e) env)])
                    (if (apair? v)
                        (apair-e1 v)
                        (error "MUPL fst applied to non-pair")))]
        [(snd? e) (let ([v (eval-under-env (snd-e e) env)])
                    (if (apair? v)
                        (apair-e2 v)
                        (error "MUPL snd applied to non-pair")))]
        [(ifgreater? e) (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
                              [v2 (eval-under-env (ifgreater-e2 e) env)])
                          (if (and (int? v1)
                                   (int? v2)
                                   (> (int-num v1) (int-num v2)))
                              (eval-under-env (ifgreater-e3 e) env)
                              (eval-under-env (ifgreater-e4 e) env)))]
        [(mlet? e) (let* ([var (mlet-var e)]
                          [exp (eval-under-env (mlet-e e) env)]
                          [env (cons (cons var exp) env)])
                     (eval-under-env (mlet-body e) env))]
        [(fun? e) (closure env e)]
        [(call? e) (let ([v1 (eval-under-env (call-funexp e) env)]
                         [v2 (eval-under-env (call-actual e) env)])
                     (if (closure? v1)
                         (let* ([fx (closure-fun v1)]
                                [env (closure-env v1)]
                                [fn (fun-nameopt fx)]
                                [fp (fun-formal fx)]
                                [fb (fun-body fx)]
                                [env (if fn 
                                         (cons (cons fn v1) (cons (cons fp v2) env)) 
                                         (cons (cons fp v2) env))])
                           (eval-under-env fb env))
                         (error "MUPL call applied to non-function")))]
        [#t (error "bad MUPL expression")]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))


(require test-engine/racket-tests)

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
;curried function map 
(define m (fun "map" "f" (fun #f "xs" (ifgreater (isaunit (var "xs")) (int 0) (aunit) (apair (call (var "f") (fst (var "xs"))) (call (call (var "map") (var "f")) (snd (var "xs"))))))))
(check-expect (eval-exp (call (call m (fun #f "n" (add (var "n") (var "n")))) (apair (int 1) (apair (int 2) (apair (int 3) (aunit)))))) (apair (int 2) (apair (int 4) (apair (int 6) (aunit)))))

;-----------------------
; Tests for problem #3
;-----------------------
(check-expect (eval-exp (ifaunit (aunit) (int 10) (int 20))) (int 10))
(check-expect (eval-exp (ifaunit (int 10) (int 20) (int 30))) (int 30))

(test)