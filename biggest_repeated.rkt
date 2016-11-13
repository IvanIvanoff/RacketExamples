#lang racket
; Да се напише функция (maxDuplicate ll), която по списък от списъци от цели числа ll
; намира най-­голямото от тези числа, които се повтарят в рамките на списъка, в който се срещат.
; Ако в нито един списък няма повтарящи се числа, функцията да връща #f.

(define (repeated lst)
  (cond[(null? lst) lst]
       [(memq (car lst) (cdr lst)) (cons (car lst) (repeated (cdr lst)))]
       [else (repeated (cdr lst))]))

(define (flatten lst)
  (cond [(null? lst) lst]
        [(list? lst)
         (append (flatten (car lst)) (flatten (cdr lst)))]
        [else (list lst)]))

(define (maxDuplicate lst)
  (cond [(null? lst) #f]
        [(null? (flatten (map repeated lst))) #f]
        [(apply max (flatten (map repeated lst)))]))
     

(maxDuplicate '((1 2 3 2) (-4 -4) (5)))
(maxDuplicate '((1 2 3) (-4 -5 -6) ())) 
