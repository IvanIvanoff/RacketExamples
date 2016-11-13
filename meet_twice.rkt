#lang racket
;  Да се напише функция (meetTwice? f g a b),
; която проверява дали в целочисления интервал [a; b] съществуват две
; различни цели числа x и y такива, че f(x) = g(x) и f(y) = g(y).

(define (from-to a b)
  (if (= a b) (cons a '())
      (cons a (from-to (+ a 1) b))))

(define (same-at-same-position lst1 lst2)
  (cond [(null? lst1) lst1]
        [(= (car lst1) (car lst2))
            (cons (car lst1) (same-at-same-position (cdr lst1) (cdr lst2)))]
        [else (same-at-same-position (cdr lst1) (cdr lst2))]))


(define (meetTwice? f g a b)
  (define lst (from-to a b))
  (define lst1 (map f lst))
  (define lst2 (map g lst))
  (>= (length (same-at-same-position lst1 lst2)) 2))
