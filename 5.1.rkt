#lang scheme
(define (complex-number-real-part n) (car n))

(define (complex-number-imaginary-part n) (cdr n))

(define (square x)
  (* x x))

(define (get-r n)
  (* 1.0 (sqrt (+ (square (complex-number-real-part n)) (square (complex-number-imaginary-part n))))))

(define (get-phi n)
  (* 1.0 (atan (complex-number-imaginary-part n) (complex-number-real-part n))))


(define (get-list-element elements n) 
  (if (= n 1) 
      (car elements)
         (get-list-element (cdr elements) (- n 1 ))
  )
)

(define (create-list n)
  (cond [(or (< n (/(length complex-numbers) 2)) (= n (/(length complex-numbers) 2)))
         (let* ([first-complex-number (get-list-element complex-numbers (- (* 2 n) 1))]
           [second-complex-number (get-list-element complex-numbers (* 2 n))]
           [a1 (car first-complex-number)]
           [b1 (cdr first-complex-number)]
           [a2 (car second-complex-number)]
           [b2 (cdr second-complex-number)]
           )
           (append (list(cons  (/ (+ (* a1 a2) (* b1 b2)) (+ (* a2 a2) (* b2 b2)))
                               (* -1 (/ (- (* a1 b2) (* b1 a2)) (+ (* a2 a2) (* b2 b2))))))
                  
                   (create-list (+ n 1)))
          )
         ]
        [else
         '()]
  )
)

(define (print-complex list n)
  (cond
    [(< n (length list))
        (begin
          (display "(")
          (if (< (car (get-list-element list n)) 0)
              (display " - ")
              (display " ")
          )
          (display (abs (car (get-list-element list n))))
          (if (< (cdr (get-list-element list n)) 0)
              (display " - ")
              (display " + ")
          )
          (display (abs (cdr (get-list-element list n))))
          (display "i ), ")
          (print-complex list (+ n 1))
        )
    ]
    [(= n (length list))
        (begin
          (display "(")
           (if (< (car (get-list-element list n)) 0)
              (display " - ")
              (display " ")
          )
          (display (abs (car (get-list-element list n))))
          (if (< (cdr (get-list-element list n)) 0)
              (display " - ")
              (display " + ")
          )
          (display (abs (cdr (get-list-element list n))))
          (display "i )")
          (print-complex list (+ n 1))
        )
    ]
   [else
    (display "\n")
   ]
  )
)

(define (print-complex-polar list n)
  (cond
    [(< n (length list))
        (begin
          (display "(")
          (display (~r (get-r (get-list-element list n)) #:precision 4))
          (display " * ( cos(")
          (display (~r (get-phi (get-list-element list n)) #:precision 4))
          (display ") + i * sin(")
          (display (~r (get-phi (get-list-element list n)) #:precision 4))
          (display ") ), ")
          (print-complex-polar list (+ n 1))
        )
    ]
    [(= n (length list))
        (begin
          (display "(")
          (display (~r (get-r (get-list-element list n)) #:precision 4))
          (display " * ( cos(")
          (display (~r (get-phi (get-list-element list n)) #:precision 4))
          (display ") + i * sin(")
          (display (~r (get-phi (get-list-element list n)) #:precision 4))
          (display ") )")
          (print-complex-polar list (+ n 1))
        )
    ]
   [else
    (display "\n")
   ]
  )
)


(define complex-numbers (list (cons 7 -4) (cons 3 2) (cons 10 -3) (cons -4 7) (cons -19 5) (cons 10 1)))
(display "The starting complex numbers list: ")
(print-complex complex-numbers 1)
(display "The new complex numbers list: ")
(print-complex (create-list 1) 1)
(display "The new complex numbers list, printed in polar form: ")
(print-complex-polar (create-list 1) 1)