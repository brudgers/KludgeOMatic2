#lang typed/racket

#| Paramaterized Type 
   Variables have a name which is a string and are parameterized 
   to use the type of whatever value is passed. See:
   http://docs.racket-lang.org/ts-guide/types.html#%28part._.Polymorphism
|#

(struct (type) Variable((name : Symbol)
                        (value : type))
  #:transparent)




