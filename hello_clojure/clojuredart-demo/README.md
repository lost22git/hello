
add deps.edn

```edn
{:paths ["src"]
 :deps {org.clojure/clojure {:mvn/version "1.12.0"}
        tensegritics/clojuredart
        {:git/url "git@github.com:tensegritics/ClojureDart.git"
         ; or  "https://github.com/tensegritics/ClojureDart.git"
         :sha "6127d8ef23156f2be5b10e97a0932868d175e585"}}
 :aliases {:cljd {:main-opts ["-m" "cljd.build"]}}
 :cljd/opts {:kind :dart
             :main lost.clojuredart-demo}}
```

```sh
mkdir src/lost
```

add `src/lost/clojuredart_demo.cljd`

```clj
(ns lost.clojuredart-demo)

(defn main []
    (println "hello"))
```

init current project as dart project 

```sh
clj -M:cljd init
```

compile cljd to dart

```sh
clj -M:cljd compile
```
watching cljd changes to compile to dart
 
```sh
clj -M:cljd watch
```

run project

```sh
dart run
```

build project

```sh
dart compile exe -o demo ./bin/clojuredart_demo.dart
```
