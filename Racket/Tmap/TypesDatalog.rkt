#lang racket

(require racklog)

(define %parent
  (%rel ()
   (('Table 'TableTop))
   (('Table 'TableLegs))
   (('TableTop 'SoftWood))
   (('TableTop 'HardWood))
   (('TableLegs 'TableLeg))
   (('TableLegs 'TableLeg))))

(define %FlatWood
  (%rel (part)
        ((part)
         (%parent part 'SoftWood)
         (%parent part 'HardWood))))

;; Returns the Symbol 'TableTop
(cdr 
 (first
  (%which (what)
          (%FlatWood what))))

;; Abstracting
(define (my-which rel)
    (cdr (first
          (%which (what)
                  (rel what)))))

;; returns 'TableTop
(my-which %FlatWood)

(%assert! %parent ()
            (('TableLegs `((TableLeg ,(range 4))))))

(define (makeSet symb n)
  (let ((r (range 1 (add1 n))))
    (map (lambda(x)(cons x symb)) r)))

(makeSet 'TableLeg 4)



