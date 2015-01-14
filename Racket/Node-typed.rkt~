#lang racket

(struct input-variable (name value) #:transparent #:mutable)
(struct input-variable-validation-rule (name function) #:transparent #:mutable)

(struct output-variable (name value) #:transparent #:mutable)
(struct output-variable-validation-rule (name function) #:transparent #:mutable)

(struct node (type
              description
              input-variables
              input-validation-rules
              children
              control-function
              output-variables
              output-validation-rules)
  #:transparent)

(provide (contract-out
          (struct node ((type symbol?)
                        (description string?)
                        (input-variables (listof input-variable?))
                        (input-validation-rules (listof input-variable-validation-rule?))
                        (children (listof node?))
                        (control-function procedure?)
                        (output-variables (listof output-variable?))
                        (output-validation-rules (listof output-variable-validation-rule?))))))
  