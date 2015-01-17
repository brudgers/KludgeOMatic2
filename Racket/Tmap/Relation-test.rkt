#lang typed/racket
(require "Relation.rkt")

;; --------
;; Leaves

(define SoftWood
  (Type null
        (list 'SoftWood)))

(define HardWood
  (Type null
        (list 'HardWood)))

(define PalmWood
  (Type null
        (list 'PalmWood)))

(define TableLeg-Wood
  (Type null
        (list 'TableLeg-Wood)))

;; Second Teir

(define TableTop-Wood
  (Type (list 'SoftWood 'HardWood)
        (list 'TableTop-Wood)))

(define TableLegs-Wood
  (Type (list 'TableLeg-Wood
              'TableLeg-Wood
              'TableLeg-Wood
              'TableLeg-Wood)
        (list 'TableLegs-Wood)))

;; Root
(define Table
  (Type (list 'TableTop-Wood
              'TableLegs-Wood)
        (list 'Table)))

