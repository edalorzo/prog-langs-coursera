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

;-----------------------
; Problem #2
;-----------------------

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
        [#t (begin 
              (print e)
              (error "bad MUPL expression"))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;-----------------------
; Problem #3
;-----------------------

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2) (if (null? lstlst)
                              e2
                              (let ([s (car (car lstlst))]
                                    [e (cdr (car lstlst))])
                                (mlet s e (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4) (mlet* (list (cons "_x" e1) (cons "_y" e2)) 
                                  (ifgreater (var "_x") (var "_y") 
                                             e4
                                             (ifgreater (var "_y") (var "_x") e4 e3))))

;-----------------------
; Problem #4
;-----------------------
(define mupl-map (fun "map" "f" 
                      (fun #f "xs" 
                           (ifaunit (var "xs") 
                                      (aunit) 
                                      (apair (call (var "f") (fst (var "xs"))) (call (call (var "map") (var "f")) (snd (var "xs"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "n" (call (var "map") (fun #f "x" (add (var "x") (var "n")))))))
        
              


;; Challenge Problem
(require racket/set)

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (define (to-vars e)
    (cond [(aunit? e) (set)]
          [(int? e) (set)]
          [(var? e) (set)]
          [(fst? e) (to-vars (fst-e e))]
          [(snd? e) (to-vars (snd-e e))]
          [(isaunit? e) (to-vars (isaunit-e e))]
          [(add? e) (set-union (to-vars (add-e1 e)) (to-vars (add-e2 e)))]
          [(apair? e) (set-union (to-vars (apair-e1 e)) (to-vars (apair-e2 e)))]
          [(ifgreater? e) (set-union (to-vars (ifgreater-e1 e))
                                     (to-vars (ifgreater-e2 e))
                                     (to-vars (ifgreater-e3 e))
                                     (to-vars (ifgreater-e4 e)))]
          [(mlet? e) (let ([vn (mlet-var e)]
                           [vr (to-vars (mlet-body e))])
                       (if (set-member? vr vn)
                           (set-remove vr vn)
                           vr))]
          [(call? e) (set-union (to-vars (call-funexp e)) (to-vars (call-actual e)))]
          [(fun? e) (to-vars (fun-body e))]
          [#t (error "bad MUPL expression")]))
  (cond [(aunit? e) e]
        [(int? e) e]
        [(var? e) e]
        [(fst? e) (fst (compute-free-vars (fst-e e)))]
        [(snd? e) (snd (compute-free-vars (snd-e e)))]
        [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
        [(add? e) (add (compute-free-vars (add-e1 e)) (compute-free-vars (add-e2 e)))]
        [(apair? e) (apair (compute-free-vars (apair-e1 e)) (compute-free-vars (apair-e2 e)))]
        [(ifgreater? e) (let([e1 (compute-free-vars (ifgreater-e1 e))]
                             [e2 (compute-free-vars (ifgreater-e2 e))]
                             [e3 (compute-free-vars (ifgreater-e3 e))]
                             [e4 (compute-free-vars (ifgreater-e4 e))])
                         (ifgreater e1 e2 e3 e4))]
        [(mlet? e) (mlet (mlet-var e) (compute-free-vars (mlet-e e)) (compute-free-vars (mlet-body e)))]
        [(call? e) (call (compute-free-vars (call-funexp e)) (compute-free-vars (call-actual e)))]
        [(fun? e) (let ([fn (fun-nameopt e)]
                        [fp (fun-formal e)]
                        [fb (compute-free-vars (fun-body e))])
                    (fun-challenge fn fp fb (to-vars e)))]
        [#t (error "bad MUPL expression")]))
          
  

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (define (from-free-vars from env)
    (if (or (null? env) (set-empty? from))
        null
        (let* ([cell (car env)]
               [name (car cell)])
          (if (set-member? from name)
              (cons cell (from-free-vars (set-remove from name) (cdr env)))
              (from-free-vars (set-remove from name) (cdr env))))))
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(isaunit? e) (let ([v (eval-under-env-c (isaunit-e e) env)])
                        (if (aunit? v) (int 1) (int 0)))]
        [(apair? e) (let ([l (eval-under-env-c (apair-e1 e) env)]
                          [r (eval-under-env-c (apair-e2 e) env)])
                      (apair l r))]
        [(fst? e) (let ([v (eval-under-env-c (fst-e e) env)])
                    (if (apair? v)
                        (apair-e1 v)
                        (error "MUPL fst applied to non-pair")))]
        [(snd? e) (let ([v (eval-under-env-c (snd-e e) env)])
                    (if (apair? v)
                        (apair-e2 v)
                        (error "MUPL snd applied to non-pair")))]
        [(ifgreater? e) (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
                              [v2 (eval-under-env-c (ifgreater-e2 e) env)])
                          (if (and (int? v1)
                                   (int? v2)
                                   (> (int-num v1) (int-num v2)))
                              (eval-under-env-c (ifgreater-e3 e) env)
                              (eval-under-env-c (ifgreater-e4 e) env)))]
        [(mlet? e) (let* ([var (mlet-var e)]
                          [exp (eval-under-env-c (mlet-e e) env)]
                          [env (cons (cons var exp) env)])
                     (eval-under-env-c (mlet-body e) env))]
        [(fun-challenge? e) (closure (from-free-vars (fun-challenge-freevars e) env) e)]
        [(call? e) (let ([v1 (eval-under-env-c (call-funexp e) env)]
                         [v2 (eval-under-env-c (call-actual e) env)])
                     (if (closure? v1)
                         (let* ([fx (closure-fun v1)]
                                [env (closure-env v1)]
                                [fn (fun-challenge-nameopt fx)]
                                [fp (fun-challenge-formal fx)]
                                [fb (fun-challenge-body fx)]
                                [env (if fn 
                                         (cons (cons fn v1) (cons (cons fp v2) env)) 
                                         (cons (cons fp v2) env))])
                           (eval-under-env-c fb env))
                         (error "MUPL call applied to non-function")))]
        [#t (error "bad MUPL expression")]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))


