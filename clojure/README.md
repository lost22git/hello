# [Clojure](https://clojure.org) Lang Learning

## Installation

https://clojure.org/guides/install_clojure

### Tools

- [babashka](https://github.com/babashka/babashka#installation)
- [conjure](https://github.com/Olical/conjure)

## Resources

- [getting_started](https://clojure.org/guides/getting_started)
- [quickref](https://clojuredocs.org/quickref)
- [clojure guide](https://clojure-doc.org)
- [elements of clojure](https://elementsofclojure.com)
- [understanding-persistent-vector-pt-1](https://hypirion.com/musings/understanding-persistent-vector-pt-1)
- [Clojure core.async.flow first look](https://redpenguin101.github.io/html/posts/2025_01_18_clojure_flows.html)


## New Project

### [deps-new](https://github.com/seancorfield/deps-new) (a clj tool)

#### add deps-new to tools as `new`

`clj -Ttools install-latest :lib <lib-name> :as <tool-name>`

```sh
clj -Ttools install-latest :lib io.github.seancorfield/deps-new :as new
```

#### list all tools

```sh
clj -Ttools list
```

#### show tool info

`clj -Ttools show :tool <tool-name>`

```sh
clj -Ttools show :tool new
```

#### view deps-new help doc

```sh
clj -A:deps -Tnew help/doc
```

#### new project

`clj -Tnew <template-name> :name <project-ns-name>/<project-name>`


```sh
clj -Tnew app :name io.github.lost/deps-new-demo
```

then create project structure:

```
.
├── .cpcache
│  ├── 2397443233.basis
│  └── 2397443233.cp
├── doc
│  └── intro.md
├── resources
│  └── .keep
├── src
│  └── lost
│     └── deps_new_demo.clj
├── test
│  └── lost
│     └── deps_new_demo_test.clj
├── .gitignore
├── build.clj
├── CHANGELOG.md
├── deps.edn
├── LICENSE
└── README.md
```

and build uberjar

```sh
clj -T:build ci
```
then create build target

```
target
├── classes
│  ├── lost
│  │  ├── deps_new_demo$_main.class
│  │  ├── deps_new_demo$fn__140.class
│  │  ├── deps_new_demo$greet.class
│  │  ├── deps_new_demo$loading__6812__auto____138.class
│  │  ├── deps_new_demo.class
│  │  ├── deps_new_demo.clj
│  │  └── deps_new_demo__init.class
│  └── .keep
└── io.github.lost
   └── deps-new-demo-0.1.0-SNAPSHOT.jar
```

```sh
unzip -p ./target/io.github.lost/deps-new-demo-0.1.0-SNAPSHOT.jar META-INF/MANIFEST.MF
```

```
Manifest-Version: 1.0
Created-By: org.clojure/tools.build
Build-Jdk-Spec: 21
Main-Class: lost.deps_new_demo
```

### neil (a babashka script to wrap deps-new)

`neil new app io.github.lost/neil-demo`

[more details](https://github.com/babashka/neil)

project structure almost same as `deps-new`

### leiningen


## Tips

### REPL

- fuzzy search functions by pattern
`(apropos "search pattern")`
```clojure
user=> (apropos "future")
(clojure.core/future clojure.core/future-call clojure.core/future-cancel clojure.core/future-cancelled? clojure.core/future-done? clojure.core/future?)
```

- view document of the function
`(doc function-name)`
```clojure
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
```clojure
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
```clojure
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
...
```

- list functions of the namespace
`(dir namespace-name)`
```clojure
user=> (dir clojure.string)                                                    blank?
capitalize
ends-with?
escape
includes?
index-of
join
last-index-of
...
```

# [ClojureScript](https://clojurescript.org) Lang Learning

# [ClojureDart](https://github.com/Tensegritics/ClojureDart) Lang Learning

## Resources

- [doc](https://github.com/Tensegritics/ClojureDart/tree/main/doc)
- [samples](https://github.com/Tensegritics/ClojureDart/tree/main/samples)

## Tips

### check alias `cljd` more functions

```sh
clj -M:cljd help
```
### use `:managed` rather than `:watch :as` to keep consistency

### use `:bind` state to Context in parent widget and `:get` state from Context in children widgets
