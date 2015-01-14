#lang typed/racket

(define-type TupleOf (-> (Listof Symbol) (-> (Listof Symbol) Boolean)))

(: tupleOf TupleOf)

(define (tupleOf list-of-symbol)
  (lambda (los) (equal? los list-of-symbol)))

(provide TupleOf
         tupleOf)

