#lang typed/racket
(require "Relation.rkt")

(define SoftWood
  (Type null
        (list 'SoftWood)))

(define HardWood
  (Type null
        (list 'HardWood)))

(define PalmWood
  (Type null
        (list 'PalmWood)))

(define TableLeg
  (Type (list 'SoftWood 'HardWood)
        (list 'TableLeg)))