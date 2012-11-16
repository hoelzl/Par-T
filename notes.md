Some notes about the implementation of Par-T
============================================

Naming Conventions
------------------

There are many things for which we currently have no consistent naming
convention in the code.  For example things relating to
functions/procedures/entities are sometimes named `*fn*`, `*fun*` or
`*proc*`.  Simlar for locales/environments.  We should clarify our
naming conventions and stick with them.

Side Effects
------------

How do we handle side effects in Par-T?


### Problem description

There should be a clean solution for deferring side effects during a
computation, so that non-deterministic computations can make use of
all faculties of the language.

### Possible solutions

One possible solution would be to introduce a notion of transaction.
But: while we can (maybe) handle all output by deferring it until the
end of a transaction, what do we do when input and output are
interleaved?  Related to this, how do we deal with distributed data
and computations in transactions?

Maybe do something similar to IndiGolog and only allow access to input
that was available before the transaction started (i.e., have
something like the observation sequence in IndiGolog).  Or force the
programmer to declare side effects as global or (transaction) local
and raise errors when they are interleaved in the wrong order
(whatever that means).

### Possible implementation

Structure the environment (locales) as follows:

	|---------------------------------|
	|    Local environment frame 0    |
	|---------------------------------|
	                 |
				    ...
				     |
	|---------------------------------|
	|    Local environment frame n    |
	|---------------------------------|
	                 |
	|---------------------------------|
	|           Dummy frame           |
	|---------------------------------|
				     |
	|---------------------------------|
	| Transaction environment frame x |
	|---------------------------------|
	                 |
	                ...
				     |
	|---------------------------------|
	| Transaction environment frame a |
	|---------------------------------|
				     |
	|---------------------------------|
	|    Global environment frame     |
	|---------------------------------|

How is this supposed to work? Restoring a continuation/thread not only
copies the stack but also links the correct sequence of transaction
environment frames below the dummy frame.  (This should be just one
assignment operation, if we are careful) Global side effects are
always recorded in the topmost transaction environment frame
(optionally allow an explicit transaction frame to be given).  When
transaction x is committed, all effects in transaction frame x are
copied to the transaction frame below; when the outermost transaction
is committed the effects are actually executed in the global
environment.


Non Determinism
---------------

How deeply should non determinism be integrated into the language?

### Problem description

When we have logical variables and constraints in the language, the
question is how deeply they should be integrated into the normal
evaluation of the language.  For example, should the following form

	(if ?x
		(do-something-when-x-is-true)
		(do-something-else-when-x-is-false))

(where `?x` is a Boolean logical variable) treat `?x` as a normal
instance of class `<logical-variable>` and always execute
`(do-something-when-x-is-true)`, or should it be treated as a
"non-deterministic value" so that

* When `?x` is known to be true then the first branch is executed

* When `?x` is known to be false then the second branch is executed

* When the value of `?x` is unknown then the expression is equivalent
  to (something like)
  
	    (choose (begin (assert '?x)
                       (do-something-when-x-is-true))
                (begin (assert '(not ?x)
                       (do-something-else-when-x-is-false))))
					   
    (this is not quite the right translation because `?x` is a
	propositional variable in the assertions, but the idea should be
	clear).
