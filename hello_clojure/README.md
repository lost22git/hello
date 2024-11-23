# Clojure Lang Learning

## Installation

https://clojure.org/guides/install_clojure

### Tools 

- [babashka](https://github.com/babashka/babashka#installation)


## Resources


## Tips

### REPL

- fuzzy search functions by pattern
`(apropos "search pattern")`
```repl
user=> (apropos "future")
(clojure.core/future clojure.core/future-call clojure.core/future-cancel clojure.core/future-cancelled? clojure.core/future-done? clojure.core/future?)
```

- view document of the function
`(doc function-name)`
```repl
user=> (doc future)
-------------------------
clojure.core/future
([& body])
Macro
  Takes a body of expressions and yields a future object that will
  invoke the body in another thread, and will cache the result and
  return it on all subsequent calls to deref/@. If the computation has
  not yet finished, calls to deref/@ will block, unless the variant of
  deref with timeout is used. See also - realized?.
```

- view source code of the function
`(source function-name)`
```repl
user=> (source future)
(defmacro future
  "Takes a body of expressions and yields a future object that will
  invoke the body in another thread, and will cache the result and
  return it on all subsequent calls to deref/@. If the computation has
  not yet finished, calls to deref/@ will block, unless the variant of
  deref with timeout is used. See also - realized?."
  {:added "1.1"}
  [& body] `(future-call (^{:once true} fn* [] ~@body)))
```

- fuzzy search docs by pattern
`(find-doc "search pattern")`
```repl
user=> (find-doc "future")
-------------------------
clojure.core/deref
([ref] [ref timeout-ms timeout-val])
  Also reader macro: @ref/@agent/@var/@atom/@delay/@future/@promise. Within a transaction,
  returns the in-transaction-value of ref, else returns the
  most-recently-committed value of ref. When applied to a var, agent
  or atom, returns its current state. When applied to a delay, forces
  it if not already forced. When applied to a future, will block if
  computation not complete. When applied to a promise, will block
  until a value is delivered.  The variant taking a timeout can be
  used for blocking references (futures and promises), and will return
  timeout-val if the timeout (in milliseconds) is reached before a
  value is available. See also - realized?.
-------------------------
clojure.core/future
([& body])
Macro
  Takes a body of expressions and yields a future object that will
  invoke the body in another thread, and will cache the result and
  return it on all subsequent calls to deref/@. If the computation has
  not yet finished, calls to deref/@ will block, unless the variant of
  deref with timeout is used. See also - realized?.
-------------------------
clojure.core/future-call
([f])
  Takes a function of no args and yields a future object that will
  invoke the function in another thread, and will cache the result and
  return it on all subsequent calls to deref/@. If the computation has
  not yet finished, calls to deref/@ will block, unless the variant
  of deref with timeout is used. See also - realized?.
-------------------------
clojure.core/future-cancel
([f])
  Cancels the future, if possible.
-------------------------
clojure.core/future-cancelled?
([f])
  Returns true if future f is cancelled
-------------------------
clojure.core/future-done?
([f])
  Returns true if future f is done
-------------------------
clojure.core/future?
([x])
  Returns true if x is a future
-------------------------
clojure.core/realized?
([x])
  Returns true if a value has been produced for a promise, delay, future or lazy sequence.
```

- list functions of the namespace
`(dir namespace-name)`
```repl
user=> (dir clojure.string)                                                    blank?
capitalize
ends-with?
escape
includes?
index-of
join
last-index-of
lower-case
re-quote-replacement
replace
replace-first
reverse
split
split-lines
starts-with?
trim
trim-newline
triml
trimr
upper-case
```

## Summary
