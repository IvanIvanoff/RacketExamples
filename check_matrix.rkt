#lang racket
; Да се напише функция (checkMatrix? m k) която проверява дали на
; всеки ред в дадена матрица m от цели числа има поне
; по едно число, кратно на k.
(define (all p? lst)
  (cond [(null? lst) #t]
        [(not (p? (car lst))) #f]
[else (all p? (cdr lst))]))

(define (any? p? lst)
  (not (all (lambda (x) (not (p? x))) lst)))

(define (div k lst)
  (any? (lambda (x) (= (remainder x k) 0)) lst))

(define (checkMatrix? m k)
  (cond[(null? m) #t]
       [(not (div k (car m))) #f]
       [else (checkMatrix? (cdr m) k)]))

(checkMatrix? '((1 2 6) (3 8 9) (10 12 11)) 3) ; should be #t
(checkMatrix? '((1 2 4) (3 8 9) (10 12 11)) 3) ; should be #f
