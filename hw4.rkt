
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

  
(define (list-nth-mod xs n)
  (cond [(empty? xs) (error "list-nth-mod: empty list")]
        [(< n 0) (error "list-nth-mod: negative number")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))


(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([cell (s)])
        (cons (car cell) (stream-for-n-steps (cdr cell) (- n 1))))))

(define funny-number-stream
  (letrec ([m (lambda (x) (if (= (remainder x 5) 0) (- x) x))]
           [f (lambda (x) (cons (m x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define (dan-the-dog)
  (define first (lambda () (cons "dan.jpg" second)))
  (define second (lambda () (cons "dog.jpg" first)))
  (first))

(define (stream-add-zero s)
  (define (add-zero t) (cons 0 (car t)))
  (lambda () (let([cell (s)])
               (cons (add-zero cell) (lambda() ((stream-add-zero (cdr cell))))))))

(define (cycle-lists xs ys)
  
  (define (to-stream xs)
    (letrec ([stream (lambda (xs n) (cons (list-nth-mod xs n) (lambda () (stream xs (+ n 1)))))])
      (lambda () (stream xs 0))))
  
  (define (combine sx sy)
    (let([to-pair (lambda (x y) (cons (car (x)) (car (y))))])
      (cons (to-pair sx sy) (lambda () (combine (cdr (sx)) (cdr (sy)))))))
  
  (lambda () (combine (to-stream xs) (to-stream ys))))
       


           



(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

    

    


    






        
  
  
                    

                    


      