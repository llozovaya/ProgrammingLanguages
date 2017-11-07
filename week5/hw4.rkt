#lang racket

(provide (all-defined-out))


(define (sequence low high stride)
  (if
   (<= low high)
   (cons low (sequence (+ low stride) high stride))
   null
   )
)

(define (string-append-map xs suffix)
  (map (lambda (str)  (string-append str suffix) ) xs)
)

(define (list-nth-mod xs n)
  (cond
     [(< n 0)
     (error "list-nth-mod: negative number")]
     [(null? xs)
     (error "list-nth-mod: empty list")]
     [#t (let
         ([n_remainder (remainder n (length xs))])
         (car (list-tail xs n_remainder))
         )]
  )
)

(define (stream-for-n-steps s n)
  (if
   (= n 0)
   null
  (let ([s1 (s)])
     (cons
      (car s1)
      (stream-for-n-steps (cdr s1) (- n 1))
     )
  )
  )
)
(define funny-number-stream
  (letrec ([f (lambda (n)
             (cons
              (if (= (remainder n 5) 0) (- 0 n) n)
              (lambda () (f (+ n 1)))
              ))
       ])
    (lambda ()
      (f 1)
    )
  )
)


(define dan-then-dog
  (letrec
      ([dan (lambda () (cons "dan.jpg" dog))]
       [dog (lambda () (cons "dog.jpg" dan))]
       )
   dan
  )
)

(define (stream-add-zero s)
  (let
      (
       [f (lambda ())]
       []
       )
   )
  
)