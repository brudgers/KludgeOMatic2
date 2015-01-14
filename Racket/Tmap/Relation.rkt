#lang typed/racket

(provide OneOf)

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


;; Define the OneOf relationship
(: oneOf TypeRelation)
(define (oneOf (parent : Type) children)
  
  (define requirements : (Listof Requirement) 
    (Type-requirement parent))
  
    ;; This is a one level flatten
  (: get-productions ((Listof (Listof Production)) . -> . (Listof Symbol)))
  (define (get-productions loa)
    (cond 
      ((null? loa) null)
      (else
       (append (first loa)
               (get-productions (rest loa))))))
  
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
  
  (ormap requirement-met? requirements))

;; Instantiate the OneOf relationship
(define OneOf
  (Relation oneOf))





