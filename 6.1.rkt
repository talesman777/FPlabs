#lang scheme
(define (get-list-element elements n) 
  (if (= n 1) 
      (car elements)
         (get-list-element (cdr elements) (- n 1 ))
  )
)

(define (pop-queue)
  (begin
    (define popped-number (car queue))
    (display "Removing ")
    (display popped-number)
    (display " from the queue.\n")
    (set! queue (cdr queue))
    (display "Current queue state: ")
    (display queue)
    (display "\n")
    popped-number
  )
)

(define (push-queue n)
  (begin
    (display "Adding ")
    (display n)
    (display " to the queue.\n")
    (set! queue (append queue (list n)))
    (display "Current queue state: ")
    (display queue)
    (display "\n")
  )
)

(define (pop-stack)
  (begin
    (define popped-number (get-list-element stack (length stack)))
    (display "Removing ")
    (display popped-number)
    (display " from the stack.\n")
    (set! stack (pop-stack-inner stack))
    (display "Current stack state: ")
    (display stack)
    (display "\n")
    popped-number
  )
)

(define (pop-stack-inner current-stack)
  (cond [(> (length current-stack) 1)
           (append (list (car current-stack)) (pop-stack-inner (cdr current-stack)))
         ]
        [else
         '()
        ]
   )
)

(define (push-stack n)
  (begin
    (display "Adding ")
    (display n)
    (display " to the stack.\n")
    (set! stack (append stack (list n)))
    (display "Current stack state: ")
    (display stack)
    (display "\n")
  )
)

(define (number-of-occurences n current-list count)
  (cond
   [(not (null? current-list))
    (if (= n (car current-list))
        (number-of-occurences n (cdr current-list) (+ 1 count))
                (number-of-occurences n (cdr current-list) count)
    )
   ]
   [else
      count
    ]
  )
)


(define (empty-the-stack)
 (cond [(empty? stack)
           (begin
             (display "The resulting stack is: ")
             (display stack)
             (display "\n")
           )
          ]
          [else
           (begin
             (define current-number (pop-stack))
             (set! temp-list (append temp-list (list current-number)))
             (empty-the-stack)
           )
          ]
  )
)

(define (fill-the-queue source)
 (cond [(empty? source)
           (begin
             (display "The resulting queue is: ")
             (display queue)
             (display "\n")
           )
          ]
          [else
           (begin
             (define current-number (car source))
             (cond [(= ( number-of-occurences current-number temp-list 0) 1)
                    (push-queue current-number)
                    ]
                   [else
                    (begin
                      (display "Skipping ")
                      (display current-number)
                      (display " as it is used more than once.\n")
                    )
                   ]
             )
             (fill-the-queue (cdr source))
           )
          ]
  )
)

(define queue (list))
(define stack (list))
(define temp-list (list))
(push-stack 1)
(push-stack 2)
(push-stack 1)
(push-stack 4)
(push-stack 5)
(push-stack 3)
(push-stack 7)
(push-stack 11)
(push-stack 3)
(push-stack 12)
(push-stack 8)
(push-stack 1)
(empty-the-stack)
(fill-the-queue temp-list)