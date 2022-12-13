#lang scheme
(define (odd-numbers current-string current-number)  
  (if (= current-number 0)
      current-string 
      (if (odd? current-number)
          (odd-numbers (string-append current-string (number->string current-number) " ") (- current-number 1))
        
         (odd-numbers current-string (- current-number 1))
       )
   )
)
(display "Завдання 14.2\n")
(display "Для введеного значення n отримано послідовність:\n")
(odd-numbers "" (read))