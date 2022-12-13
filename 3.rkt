#lang scheme
(define error 1e-5)

(define (f x)
  (+ (exp x) (* 10 x))
)
(define (x-func x)
  (/ (exp x) -10)
)
(define (f-derivative x)
  (+ (exp x) 10)
)

(define (fixed-point-iteration current-x)
  (let ((next-x (x-func current-x)))
    (if (< (abs (- next-x current-x)) error)
      current-x
      (fixed-point-iteration next-x)
      )
  )
)

(define (newton-method current-x)
  (let ((next-x (- current-x (/(f current-x) (f-derivative current-x)))))
    (if (< (abs (- next-x current-x)) error)
        current-x  
      (newton-method next-x)
      )
  )
)

(define fixed-point-iteration-solution (fixed-point-iteration 4))
(define newton-solution (newton-method 4))
(define solution-difference (abs (- fixed-point-iteration-solution newton-solution)))

(display "Solution using the Fixed-Point Iteration method: ")
(display fixed-point-iteration-solution)
(display "\n")
(display "Solution using the Newton method: ")
(display newton-solution)
(display "\n")
(display "Absolute difference between the solutions: ")
(display solution-difference)
