#lang typed/racket

(define-type SetOf (-> Symbol Positive-Integer (-> Symbol Positive-Integer Boolean)))

(: setOf SetOf)
(define (setOf type quantity)
  (lambda(t q) 
    (and (equal? t type)
         (equal? q quantity))))

(provide SetOf
         setOf)
