#lang typed/racket

(: member? (-> Symbol (Listof Symbol) Boolean))

(define (member? item collection)
  (cond ((null? collection) #f)
        ((equal? item (car collection)) #t)
        (else (member? item (cdr collection)))))

#|
OneOf function takes an input and checks to see if it is a valid type.
|#

(: OneOf (-> (Listof Symbol) (-> Symbol Boolean)))
(define (OneOf list-of-types)
  (lambda (type)
    (member? type list-of-types)))

(provide OneOf)
             