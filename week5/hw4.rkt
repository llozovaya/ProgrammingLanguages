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
  (if
     (null? xs)
     (error "list-nth-word: empty list")
     (let
         ([n_remainder (remainder n (length xs))])
         (car (list-tail xs n_remainder))
         )
  )
)


stream-for-
