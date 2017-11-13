#lang racket

(provide (all-defined-out))

; Problem 1
(define (sequence low high stride)
  (if
   (<= low high)
   (cons low (sequence (+ low stride) high stride))
   null
   )
)

; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (str)  (string-append str suffix) ) xs)
)

; Problem 3
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

; Problem 4
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

; Problem 5
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

; Pronlem 6
(define dan-then-dog
  (letrec
      ([dan (lambda () (cons "dan.jpg" dog))]
       [dog (lambda () (cons "dog.jpg" dan))]
       )
   dan
  )
)

; Problem 7
(define (stream-add-zero s)
  (lambda ()
    (let ([s1 (s)])
      (cons (cons 0 (car s1)) (stream-add-zero (cdr s1)))
    )
  )
)


; Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))) )])
   (lambda ()
    (f 0)
    )
  )
)

; Problem 9
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (i) (let ([vi (vector-ref vec i)])
                           (if (and (pair? vi) (= (car vi) v))
                               vi
                               (if (= i (- len 1))
                                  #f
                                  (f (+ i 1))
                               )
                           ) ))])  
  (f 0)
  )
)

; Problem 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [pointer 0]
           )
   (lambda (v)
    (let ([cached (vector-assoc v cache)])
      (if cached cached
        (let ([ans (assoc v xs)])
          (if ans
          (begin (vector-set! cache pointer ans) (set! pointer (if (= pointer (- n 1)) 0 (+ pointer 1)) ) ans)
          ans
          )
          )
      )
    )
   )
))