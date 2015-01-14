*Since I am self answering, I'm taking the liberty to clarify the gist of my question in light of the discussion of arity as a solution. The difference in arity was due to my not considering its implications when specifying the question.*

In `#lang typed/racket` as in many Lisps, functions, or more properly: [procedures][1], are first class dataypes. By default, `#lang racket` types procedures by [arity][4] and any additional specificity in argument types must be done by contract. In `#lang typed/racket` procedures are typed both by arity and by the types of their arguments and return values due to the language's "baked-in contracts".

##The Problem

The [Typed Racket Guide][2] provides an [example][3] using [`define-type`][5] to define a procedure type:

     (define-type NN (-> Number Number))

This allows specifying a procedure more succinctly:

     ;; Takes two numbers, returns a number
	 (define-type 2NN (-> Number Number Number))

     (: trigFunction1 2NN)
	 (define (trigFunction1 x s)
	   (* s (cos x)))

     (: quadraticFunction1 2NN)
	 (define (quadraticFunction1 x b)
	   (let ((x1 x))
	     (+ b (* x1 x1))))

###Math as an example
In a domain like mathematics, it would be nice to work with more abstract procedure types because knowing that a function is cyclical between upper and lower bounds (like `cos`) versus having only one bound (e.g. our quadratic function) versus asymptotic (e.g. a hyperbolic function) provides for clearer reasoning about the problem domain. I'd like access to useful abstractions something like:

     (define-type Cyclic2NN (-> Number Number Number))
     (define-type SingleBound2NN (-> Number Number Number))

     (: trigFunction1 Cyclic2NN)
	 (define (trigFunction1 x s)
	   (* s (cos x)))

     (: quadraticFunction1 SingleBound2NN)
	 (define (quadraticFunction1 x b)
	   (let ((x1 x))
	     (+ b (* x1 x1))))

     (: playTone (-> Cyclic2NN))
	 (define (playTone waveform)
	   ...)

     (: rabbitsOnFarmGraph (-> SingleBound2NN)
	 (define (rabbitsOnFarmGraph populationSize)
	   ...)

Alas, `define-type` does not deliver this level of granularity when it comes to procedures. Even moreover, the brief false hope that we might easily wring such type differentiation for procedures manually using [`define-predicate`][6] is dashed by:

> Evaluates to a predicate for the type t, with the type (Any -> Boolean : t). t may not contain function types, or types that may refer to mutable data such as (Vectorof Integer).

Fundamentally, types have uses beyond static checking and contracts. As first class members of the language, we want to be able to dispatch our finer grained procedure types.  Conceptually, what is needed are predicates along the lines of `Cyclic2NN?` and `SingleBound2NN?`. Having only arity for dispatch using [`case-lambda`][7] just isn't enough.

##Guidance from Untyped Racket
Fortunately, Lisps are domain specific languages for writing Lisps once we peal back the curtain to reveal the wizard, and in the end we can get what we want. The key is to come at the issue the other way and ask "How canwe use the predicates `typed/racket` gives us for procedures?"

Structures are Racket's user defined data types and are the basis for extending it's type system. Structures are so powerful that even in the class based object system, "[classes and objects are implemented in terms of structure types][8]."

In `#lang racket` structures can be applied as procedures giving the `#:property` keyword using `prop:procedure` followed by a procedure for it's value. The documentation provides two examples:

The [first example][9] specifies a field of the structure to be applied as a procedure. Obviously, at least once it has been pointed out, that field must hold a value that evaluates to a procedure.

    > ;; #lang racket
	> (struct annotated-proc (base note)
         #:property prop:procedure
          (struct-field-index base))
    > (define plus1 (annotated-proc
         (lambda (x) (+ x 1))
         "adds 1 to its argument"))
	> (procedure? plus1)
    #t
    > (annotated-proc? plus1)
    #t
	> (plus1 10)
	11
	> (annotated-proc-note plus1)
	"adds 1 to its argument"

In the [second example][10] an anonymous procedure [lambda] is provided directly as part of the property value. The lambda takes an operand in the first position which is resolved to the value of the structure being used as a procedure. This allows accessing any value stored in any field of the structure including those which evaluate to procedures.

    > ;; #lang racket
	> (struct greeter (name)
        #:property prop:procedure
        (lambda (self other)
           (string-append
             "Hi " other
              ", I'm " (greeter-name self))))
	> (define joe-greet (greeter "Joe"))
	> (greeter-name joe-greet)
	"Joe"
	> (joe-greet "Mary")
	"Hi Mary, I'm Joe"
	> (joe-greet "John")
	"Hi John, I'm Joe

##Applying it to typed/racket
Alas, neither syntax works with [`struct`][11] as implemented in `typed/racket`. The problem it seems is that the static type checker as currently implemented cannot both define the structure and resolve its signature as a procedure at the same time. The right information does not appear to be available at the right phase when using `typed/racket`'s `struct` special form.

To get around this, `typed/racket` provides [`define-struct/exec`][12] which roughly corresponds to the second syntactic form from `#lang racket` less the keyword argument and property definition:

        (define-struct/exec name-spec ([f : t] ...) [e : proc-t])
		
	      name-spec	 	=	 	name
     	            	|	 	(name parent)

> Like define-struct, but defines a procedural structure. The procdure e is used as the value for prop:procedure, and must have type proc-t.

Not only does it give us strongly typed procedural forms, it's a bit more elegant than the keyword syntax found in `#lang racket`. Example code to resolve the question as restated here in this answer is:


    #lang typed/racket

    (define-type 2NN (-> Number Number Number))

	(define-struct/exec Cyclic2NN
       ((f : 2NN))
       ((lambda(self x s)
         ((Cyclic2NN-f self) x s))
          : (-> Cyclic2NN Number Number Number)))

     (define-struct/exec SingleBound2NN
       ((f : 2NN))
       ((lambda(self x s)
         ((SingleBound2NN-f self) x s))
           : (-> SingleBound2NN Number Number Number)))

     (define trigFunction1 
       (Cyclic2NN 
        (lambda(x s) 
          (* s (cos x)))))

	(define quadraticFunction1
	  (SingleBound2NN
	    (lambda (x b)
          (let ((x1 x))
            (+ b (* x1 x1)))))


The defined procedures are strongly typed in the sense that:

    > (SingleBound2NN? trigFunction1)
    - : Boolean
    #f
    >  (SingleBound2NN? quadraticFunction1)
    - : Boolean
    #t

All that remains is writing a macro to simplify specification.

[1]:http://docs.racket-lang.org/reference/procedures.html?q=proc-t
[2]:http://docs.racket-lang.org/ts-guide/index.html
[3]:http://docs.racket-lang.org/ts-guide/more.html#%28part._.New_.Type_.Names%29
[4]:http://en.wikipedia.org/wiki/Arity
[5]:http://docs.racket-lang.org/ts-reference/special-forms.html#%28form._%28%28lib._typed%2Fracket%2Fbase..rkt%29._define-type%29%29
[6]:http://docs.racket-lang.org/ts-reference/special-forms.html?q=make-predicate#%28form._%28%28lib._typed-racket%2Fbase-env%2Fprims..rkt%29._make-predicate%29%29
[7]:http://docs.racket-lang.org/ts-reference/special-forms.html?q=case-lambda#%28form._%28%28lib._typed-racket%2Fbase-env%2Fprims..rkt%29._case-lambda%29%29
[8]:http://docs.racket-lang.org/guide/define-struct.html
[9]:http://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._prop~3aprocedure%29%29
[10]:http://docs.racket-lang.org/guide/define-struct.html#%28part._struct-options%29
[11]:http://docs.racket-lang.org/ts-reference/special-forms.html#%28form._%28%28lib._typed-racket%2Fbase-env%2Fprims..rkt%29._struct%29%29
[12]:http://docs.racket-lang.org/ts-reference/special-forms.html#%28form._%28%28lib._typed-racket%2Fbase-env%2Fprims..rkt%29._define-struct%2Fexec%29%29
