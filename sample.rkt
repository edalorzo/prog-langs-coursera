#lang racket
(provide (all-defined-out))

(define m 3)
(define n (+ m 2))

(define cube1
  (lambda (x)
    (* x (* x x))))

(define cube2
  (lambda (x)
    (* x x x)))

(define pow
  (lambda (x y)
    (if (= y 0)
        1
        (* x (pow x (- y 1))))))


(define (cube3 x)
  (* x x x))

(define (pow2 x y)
  (if (= y 0)
      1
      (* x (pow2 x (- y 1)))))


(define pow3
  (lambda (x)
    (lambda (y)
      (if (= y 0)
          1
          (* x ((pow3 x) (- y 1)))))))

(define three-to-the (pow3 3))
(define eightyone (three-to-the 4))
(define sixteen ((pow3 2) 4))

(define ((pow4 x) y)
  (if (= y 0)
      1
      (* x ((pow4 x) (- y 1)))))

(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (map f (cdr xs)))))

(define (sum1 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum1 (cdr xs)))
          (+ (sum1 (car xs)) (sum1 (cdr xs))))))

(define (sum2 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum2 (cdr xs)))]
        [#t (+ (sum2 (car xs)) (sum2 (cdr xs)))]))


(define (sum3 xs)
  (cond [(null? xs) 0]
        [(number? xs) xs]
        [(list? xs) (+ (sum3 (car xs)) (sum2 (cdr xs)))]
        [#t (sum3 (cdr xs))]))

(define (silly-double x)
  (let ([x (+ x 3)]
        [y (+ x 2)])
    (+ x y -5)))

(define (silly-double2 x)
  (let* ([x (+ x 3)]
         [y (+ x 2)])
    (+ x y -8)))

(define (triple x)
  (letrec ([y (+ x 2)]
           [f (lambda (z) (+ z y w))]
           [w (+ x 7)])
    (f -9)))

(define (mod2 x)
  (letrec
      ([even? (lambda (x) (if (zero? x) #t (odd? (- x 1))))]
       [odd? (lambda (x) (if (zero? x) #f (even? (- x 1))))])
    (if (even? x) 0 1)))

(define (mod2b x)
  (define even? (lambda (x) (if (zero? x) #t (odd? (- x 1)))))
  (define odd? (lambda (x) (if (zero? x) #f (even? (- x 1)))))
  (if (even? x) 0 1))

(define b 3)
(define f (lambda (x) (* 1 (+ x b))))
(define c (+ b 4))
(set! b 5)
(define z (f 4))
(define w c)

;an improper list
(define t (pair? (cons (+ 7 7) #t)))

(define (my-if-bad x y z) (if x y z))

(define (factorial-wrong x)
  (my-if-bad (= x 0)
             1
             (* x (factorial-wrong (- x 1)))))

(define (my-if x y z) (if x (y) (z)))

;using thunks
(define (factorial x)
  (my-if (= x 0)
         (lambda () 1)
         (lambda () (* x (factorial (- x 1))))))

;a cool way to use thunks
(define (my-delay f)
  (mcons #f f))

(define (my-force th)
  (if (mcar th)
      (mcdr th)
      (begin (set-mcar! th #t)
             (set-mcdr! th ((mcdr th)))
             (mcdr th))))

;Now calling (my-mult e1 e2) evaluates e1 and e2 once each and then does 0 or more additions. But what
;if e1 evaluates to 0 and e2 takes a long time to compute? Then evaluating e2 was wasteful. 
(define (my-mult x y)
  (cond [(= x 0) 0]
        [(= x 1) y]
        [#t (+ y (my-mult (- x 1) y))]))

;So we could thunk it.
;This is a thunked version of my-mult
(define (my-mult2 x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult2 (- x 1) y-thunk))]))

;since we are still evaluating y-thunk severla times, we could use my-delay to cache
;the results of the first evaluation of y-thunk as follows
(define (my-mult3 x y-thunk)
  (let ([y (my-delay y-thunk)])
    (my-mult2 x (lambda () (my-force y)))))


(define ones (lambda () (cons 1 ones)))

(define (take n stream)
  (if (= n 0)
      null
      (cons (car (stream)) (take (- n 1) (cdr (stream))))))


(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x x)))))])
    (lambda () (f 2))))

(define (take-while p xs)
  (let* ([v (car xs)])
    (if (p v)
        (cons v (take-while p ((cdr xs))))
        null)))

; sample call for take-while
;(take-while (lambda (x) (<= x 15)) (nats))

(define fact
  (letrec ([f (lambda (n acc) (cons (* n acc) (lambda () (f (+ n 1) (* n acc)))))])
    (lambda () (f 1 1))))

;sample call using fact
;(take-while (lambda (x) (<= x 1000)) (fact))









    
                        





             
  




        

                    




      

  
