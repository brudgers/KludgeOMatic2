#lang typed/racket

(provide OneOf
         SetOf
         TupleOf
         Type)

;; Defines Type for use in Tmaps

(define-type Requirement  Symbol)
(define-type Production  Symbol)


(struct Type ((requirement : (Listof Requirement))
              (production : (Listof Production)))
  #:transparent)


;; The Type is the parent.
;; The (Listof Type) are the children.
;; Should return #t if the interfaces are in alignment.
(define-type TypeRelation
  (-> Type (Listof Type) Boolean))

;; Relation is a structure that is also a procedure.
;; Currently it only implements interface checking for the Tmap.
;; #TODO extend to produce Objects for the Omap.

(define-struct/exec Relation
  ((interface : TypeRelation))
  ((lambda((self : Relation)
           (parent : Type)
           (children : (Listof Type))) 
     ((Relation-interface self) parent children)) 
   : (Relation Type (Listof Type) . -> . Boolean)))


;; ---------
;; Relations

;; Define the OneOf relationship
(: oneOf TypeRelation)
(define (oneOf parent children)
  
  (define requirements : (Listof Requirement) 
    (Type-requirement parent))
  
  (define productions : (Listof Production)
    (get-productions (map Type-production children)))
    
  (: requirement-met? (Requirement . -> . Boolean))
  (define (requirement-met? requirement)  
  
    (: member? (Requirement (Listof Production) . -> . Boolean))
    (define (member? item collection)
      (cond ((null? collection) #f)
            ((equal? item (car collection)) #t)
            (else (member? item (cdr collection)))))
    
    (member? requirement productions))  
  
  (andmap requirement-met? requirements))


;; Define setOf Relationship
;; TODO validate no duplicates in requirements and productions
;; If duplicates are OK in set, then set should be atomic
;; Need to think about bag versus set.
(: setOf TypeRelation)
(define (setOf parent children)
  (define requirements : (Listof Requirement) 
    (symbol-sort (Type-requirement parent)))
  
  (define productions : (Listof Production)
    (symbol-sort (get-productions (map Type-production children))))
  
  (equal? requirements productions))

;; Define tupleOf Relationship
(: tupleOf TypeRelation)
(define (tupleOf parent children)
  
  (define requirements : (Listof Requirement) 
    (Type-requirement parent))
  
  (define productions : (Listof Production)
    (get-productions (map Type-production children)))
  
  ;; Order of the Children matters
  (equal? requirements productions))


;; Instantiate relationships
(define OneOf
  (Relation oneOf))

(define SetOf
  (Relation setOf))

(define TupleOf
  (Relation tupleOf))

;; ---------------
;; Utility Function

;; This is a one level flatten
(: get-productions ((Listof (Listof Production)) . -> . (Listof Production)))
(define (get-productions loa)
  (cond 
    ((null? loa) null)
    (else
     (append (first loa)
             (get-productions (rest loa))))))

;; Arrange (Listof Symbol) in Lexographic Order
(: symbol-sort ((Listof Symbol) . -> . (Listof Symbol)))
(define (symbol-sort los)
  (: sorter (Symbol Symbol . -> . Boolean)) 
  (define (sorter x y)
    (string<? (symbol->string x)
              (symbol->string y)))
  (sort los sorter))







