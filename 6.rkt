#lang scheme
(define (find-maximum-negative vector)
  (find-maximum-negative-internal vector 0)
)

(define (find-maximum-negative-internal vector index)
  (cond  [(< index (vector-length vector))
        (begin
         (define current-number (vector-ref vector index))
         (cond 
           [(and (not (pair? maximum-negative))  (< current-number 0))
                 (set! maximum-negative (cons index current-number))
                 ]
           [(and (pair? maximum-negative) (< (cdr maximum-negative) current-number) (< current-number 0))
                 (set! maximum-negative (cons index current-number))
                 ]
          )
          (find-maximum-negative-internal vector (+ 1 index))
        )
    ]
   )
)

(define (find-minimum-positive vector)
  (find-minimum-positive-internal vector 0)
)

(define (find-minimum-positive-internal vector index)
  (cond  [(< index (vector-length vector))
        (begin
         (define current-number (vector-ref vector index))
         (cond  
           [(and (not (pair? minimum-positive))  (> current-number 0))
                 (set! minimum-positive (cons index current-number))
                 ]
          
           [(and (pair? minimum-positive) (> (cdr minimum-positive) current-number) (> current-number 0))
                 (set! minimum-positive (cons index current-number))
                 ]
          )
          (find-minimum-positive-internal vector (+ 1 index))
        )
    ]
   )
)

(define (print-results)
  (begin
    (display "The initial vector: ")
    (display number-vector)
    (display "\n")
    (cond  [(pair? maximum-negative)
              (begin
                (display "The maximum negative number: ")
                (display (cdr maximum-negative))
                (display ". Its index within the list: ")
                (display (car maximum-negative))
              )
            ]
           [else
              (begin
                (display "Could not find the maximum negative number as there are no negative numbers in the vector.")
              )
            ]
     )
    (display "\n")
        (cond  [(pair? minimum-positive)
              (begin
                (display "The minimum positive number: ")
                (display (cdr minimum-positive))
                (display ". Its index within the list: ")
                (display (car minimum-positive))
              )
            ]
           [else
              (begin
                (display "Could not find the minimum positive number as there are no positive numbers in the vector.")
              )
            ]
     )
 )
)

(define number-vector #(-12 -32 -5 -3 -2 -6 -15 -4 -16))
(define maximum-negative '())
(define minimum-positive '())
(find-maximum-negative number-vector)
(find-minimum-positive number-vector)
(print-results)