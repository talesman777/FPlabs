#lang scheme
(define (get-list-element elements n) 
  (if (= n 1) 
      (car elements)
         (get-list-element (cdr elements) (- n 1 ))
  )
)

(define (create-list n)
  (cond [(or (< n (/(length numbers) 2)) (= n (/(length numbers) 2)))
         (let ([first-fraction (get-list-element numbers (- (* 2 n) 1))]
           [second-fraction (get-list-element numbers (* 2 n))])
          
           (append (list(cons (*(car first-fraction) (car second-fraction)) (*(cdr first-fraction) (cdr second-fraction))))
                   (create-list (+ n 1)))
          )
         ]
     
        [else
         '()]
  )
)

(define (print-fractions list n)
  (cond
    [(< n (length list))
        (begin
          (display (car (get-list-element list n)))
          (display "/")
          (display (cdr (get-list-element list n)))
          (display ", ")
          (print-fractions list (+ n 1))
        )
    ]
    [(= n (length list))
        (begin
          (display (car (get-list-element list n)))
          (display "/")
          (display (cdr (get-list-element list n)))
          (print-fractions list (+ n 1))
        )
    ]
    [else
     (display "\n")
    ]
  )
)

(define numbers (list (cons 1 2) (cons 5 4) (cons 7 13) (cons 4 19)))
(display "The starting fractions list: ")
(print-fractions numbers 1)
(display "The new fractions list: ")
(print-fractions (create-list 1) 1)