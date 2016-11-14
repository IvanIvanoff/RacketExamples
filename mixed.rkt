#lang racket
; Да се напише функция (mixed? f g a b), която проверява дали в целочисления интервал [a; b]
;съществуват цели числа x и y такива, че f(x) < g(x), но f(y) > g(y).

(define (all p? a b)
  (cond [(> a b) #t]
        [(not (p? a)) #f]
        [else (all p? (+ 1 a) b)]))

(define (any? p? a b)
  (not (all (lambda (x) (not (p? x))) a b)))

(define (mixed? f g a b)
  (and (any? (lambda (x) (< (f x) (g x))) a b)
       (any? (lambda (y) (> (f y) (g y))) a b)))

(mixed? (lambda (x) x) (lambda (x) (- x)) -3 1)

(mixed? sqrt exp 1 5)
