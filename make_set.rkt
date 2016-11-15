#lang racket
(define (isSeen? el lst)
  (< 0 (length (filter (lambda (x) (= x el)) lst))))

(define (make-set  lst)
  (define (ms set l)
    (cond [(null? l) set]
          [(isSeen? (car l) set) (ms set (cdr l))]
          [else (ms (cons (car l) set) (cdr l))]))
  (ms '() lst))
        
