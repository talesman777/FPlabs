#lang scheme
(define (power b p depth)
  (if (= p 0)
      (begin (display "\n Глибина рекурсії: ") (display depth)  (display "\n") 1)
      (if(odd? p)
         (* b (power b (/ (- p 1) 2) (+ depth 1)) (power b (/ (- p 1) 2) (+ depth 1)))
         (* (power b (/ p 2)  (+ depth 1)) (power b (/ p 2)  (+ depth 1)))
       )
  )
)

(define (func b p m) 
(modulo (power b p 0) m))


(display "Завдання 14.1")
(display "Для введених значень змінних b, p та m отримано результат:")
(func (read) (read) (read))