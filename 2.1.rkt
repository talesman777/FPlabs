#lang scheme
(define (factorial value)
  (if (= value 0)
      1
      
      (* value (factorial (- value 1)))
  )
)

(define (c n k)
  (/(factorial (+ n k -1)) (*(factorial k) (factorial (- n 1))))
)

(define (result n)
  (*(c 3 n) (factorial n))
)

(display "Введіть кількість атлетів:\n")
(define input (read))
(fprintf (current-output-port)
           "~a спортсменів можна розподілити між 3 видами спорту ~s способами.\n"
           input
           (result input))