#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file


;; put your code below

;-----------------------
; Problem #1
;-----------------------
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;-----------------------
; Problem #2
;-----------------------
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))


;-----------------------
; Problem #3
;-----------------------
(define (list-nth-mod xs n)
  (cond [(empty? xs) (error "list-nth-mod: empty list")]
        [(< n 0) (error "list-nth-mod: negative number")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

;-----------------------
; Problem #4
;-----------------------
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([cell (s)])
        (cons (car cell) (stream-for-n-steps (cdr cell) (- n 1))))))

;-----------------------
; Problem #5
;-----------------------
(define funny-number-stream
  (letrec ([m (lambda (x) (if (= (remainder x 5) 0) (- x) x))]
           [f (lambda (x) (cons (m x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;-----------------------
; Problem #6
;-----------------------
(define dan-then-dog
  (letrec ([first (lambda() (cons "dan.jpg" second))]
           [second (lambda() (cons "dog.jpg" first))])
    first))

;-----------------------
; Problem #7
;-----------------------
(define (stream-add-zero s)
  (letrec ([f (lambda (s) (let([x (s)])
                         (cons (cons 0 (car x)) (lambda() (f (cdr x))))))])
    (lambda () (f s))))
                             
;-----------------------
; Problem #8
;-----------------------
(define (cycle-lists xs ys)
  
  (define (to-stream xs)
    (letrec ([stream (lambda (xs n) (cons (list-nth-mod xs n) (lambda () (stream xs (+ n 1)))))])
      (lambda () (stream xs 0))))
  
  (define (combine sx sy)
    (let ([x (sx)]
          [y (sy)])
      (cons (cons (car x) (car y)) (lambda() (combine (cdr x) (cdr y))))))          
  
  (lambda () (combine (to-stream xs) (to-stream ys))))
       
;-----------------------
; Problem #9
;-----------------------
(define (vector-assoc v vec)
  (define (iter vec n)
    (if (>= n (vector-length vec))
        #f
        (let([x (vector-ref vec n)])
          (if (and (pair? x) (equal? (car x) v))
              x
              (iter vec (+ n 1))))))
  (iter vec 0))


(require test-engine/racket-tests)
;-----------------------
; Tests for problem #1
;-----------------------
(check-expect (sequence 3 11 2) (list 3 5 7 9 11))
(check-expect (sequence 3 8 3) (list 3 6))
(check-expect (sequence 3 2 1) null)

;-----------------------
; Tests for problem #2
;-----------------------
(check-expect (string-append-map (list "mail" "work" "view") "ing") (list "mailing" "working" "viewing"))
(check-expect (string-append-map null "not") null)

;-----------------------
; Tests for problem #3
;-----------------------
(check-expect (list-nth-mod (list 1 2 3 4 5 6) 0) 1)
(check-expect (list-nth-mod (list 1 2 3 4 5 6) 1) 2)
(check-expect (list-nth-mod (list 1 2 3 4 5 6) 2) 3)
(check-expect (list-nth-mod (list 1 2 3 4 5 6) 3) 4)
(check-expect (list-nth-mod (list 1 2 3 4 5 6) 4) 5)
(check-expect (list-nth-mod (list 1 2 3 4 5 6) 5) 6)
(check-expect (list-nth-mod (list 1 2 3 4 5 6) 6) 1)
(check-expect (list-nth-mod (list 1 2 3 4 5 6) 7) 2)
(check-expect (list-nth-mod (list 1 2 3 4 5 6) 8) 3)
(check-expect (list-nth-mod (list 1 2 3 4 5 6) 9) 4)
(check-expect (list-nth-mod (list 1 2 3 4 5 6) 10) 5)
(check-expect (list-nth-mod (list 1 2 3 4 5 6) 11) 6)
(check-error (list-nth-mod null 1) "list-nth-mod: empty list")
(check-error (list-nth-mod (list 1) -1) "list-nth-mod: negative number")

;-----------------------
; Tests for problem #4
;-----------------------
(define ones (lambda () (cons 1 ones)))
(check-expect (stream-for-n-steps ones 0) null)
(check-expect (stream-for-n-steps ones 1) (list 1))
(check-expect (stream-for-n-steps ones 2) (list 1 1))
(check-expect (stream-for-n-steps ones 3) (list 1 1 1))
(check-expect (stream-for-n-steps ones 4) (list 1 1 1 1))
(check-expect (stream-for-n-steps ones 5) (list 1 1 1 1 1))

;-----------------------
; Tests for problem #5
;-----------------------
(check-expect (list-nth-mod (stream-for-n-steps funny-number-stream 5) 4) -5)
(check-expect (list-nth-mod (stream-for-n-steps funny-number-stream 10) 9) -10)
(check-expect (list-nth-mod (stream-for-n-steps funny-number-stream 15) 14) -15)
(check-expect (list-nth-mod (stream-for-n-steps funny-number-stream 20) 19) -20)

;-----------------------
; Tests for problem #6
;-----------------------
(check-expect (stream-for-n-steps dan-then-dog 1) (list "dan.jpg"))
(check-expect (stream-for-n-steps dan-then-dog 2) (list "dan.jpg" "dog.jpg"))
(check-expect (stream-for-n-steps dan-then-dog 3) (list "dan.jpg" "dog.jpg" "dan.jpg"))
(check-expect (stream-for-n-steps dan-then-dog 4) (list "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg"))
(check-expect (stream-for-n-steps dan-then-dog 5) (list "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg"))

;-----------------------
; Tests for problem #7
;-----------------------
(check-expect (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1)))
(check-expect (stream-for-n-steps (stream-add-zero ones) 2) (list (cons 0 1) (cons 0 1)))
(check-expect (stream-for-n-steps (stream-add-zero ones) 3) (list (cons 0 1) (cons 0 1) (cons 0 1)))

;-----------------------
; Tests for problem #8
;-----------------------
(check-expect (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 1) (list (cons 1 "a")))
(check-expect (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 2) (list (cons 1 "a") (cons 2 "b")))
(check-expect (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")))
(check-expect (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 4) (list (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b")))
(check-expect (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 5) (list (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b") (cons 2 "a")))

;-----------------------
; Tests for problem #9
;-----------------------
(check-expect (vector-assoc 0 (vector)) #f) 
(check-expect (vector-assoc 1 (vector 1 2 3 4 5)) #f) 
(check-expect (vector-assoc 3 (vector (cons 1 2))) #f)
(check-expect (vector-assoc 3 (vector (cons 3 2))) (cons 3 2))
(check-expect (vector-assoc 3 (vector 1 2 3 4 5 (cons 3 2))) (cons 3 2))

(test)
