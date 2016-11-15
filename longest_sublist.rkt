#lang racket
; Да се напише функция (longestDescending­ l), която намира низходящо сортиран подсписък
; на списъка от числа l с максимална дължина. Ако съществуват няколко такива подсписъка,
; функцията да върне първия отляво надясно.

; Решение 1

(define get-n-items
    (lambda (lst num)
        (if (> num 0)
            (cons (car lst) (get-n-items (cdr lst) (- num 1)))
            '()))) ;'

(define slice
    (lambda (lst start count)
        (if (> start 1)
            (slice (cdr lst) (- start 1) count)
            (get-n-items lst count))))

(define (longestDescending­ l)
  (define (ldesc lst curr pos len max_pos max_len)
    (cond [(or (null? lst) (null? (cdr lst))) (list max_pos max_len)]
          [(> (car lst) (car (cdr lst)))
           (if (> (+ 1 len) max_len)
               (ldesc (cdr lst) (+ 1 curr) pos (+ 1 len) pos (+ 1 len))
               (ldesc (cdr lst) (+ 1 curr) pos (+ 1 len) max_pos max_len))]
          [else (ldesc (cdr lst) (+ 1 curr) (+ 1 curr) 1 max_pos max_len)]))

  (slice l (car (ldesc l 1 1 1 1 1)) (car (cdr (ldesc l 1 1 1 1 1))))
  )

(longestDescending­ '(5 3 8 6 4 2 6 7 1))

(longestDescending­ '(1 2 3 4 5 6))

; Решение 2 - shorter


(define (take-from-to lst a b)
  (if (> a b) '()
      (drop (take lst b) a)))

(define (maxAsc lst)
  (define (ma l curr pos len max_pos max_len)
    (cond [(or (null? l) (null? (cdr l)))
               (take-from-to lst max_pos (+ max_pos max_len))]
               ; (cons max_pos max_len)]
          [(< (car l) (car (cdr l)))
               (if (> (+ 1 len) max_len)
                   (ma (cdr l) (+ 1 curr) pos (+ 1 len) pos (+ 1 len))
                   (ma (cdr l) (+ 1 curr) pos (+ 1 len) max_pos max_len))]
          [else (ma (cdr l) (+ 1 curr) (+ 1 curr) 1 max_pos max_len)]))

  (ma lst 0 0 1 1 1))

(maxAsc '(1 2 3 1 4 5))
