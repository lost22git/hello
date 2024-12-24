
# Step

create project root dir and enter it

```sh
mkdir clojureflutter-demo && cd clojureflutter-demo
```
add `deps.edn`

```clj
{:paths ["src"]
 :deps {tensegritics/clojuredart
        {:git/url "https://github.com/tensegritics/ClojureDart.git"
         :sha "6127d8ef23156f2be5b10e97a0932868d175e585"}}
 :aliases {:cljd {:main-opts ["-m" "cljd.build"]}}
 :cljd/opts {:kind :flutter
             :main lost.clojureflutter-demo}}
```

init flutter project

```sh
clj -M:cljd init
```

add `src/lost/clojureflutter_demo.cljd`

```clj
(ns lost.clojureflutter-demo
  (:require ["package:flutter/material.dart" :as m]
            [cljd.flutter :as f]))

(defn main []
  (f/run
   (m/MaterialApp
    .title "Welcome to Flutter"
    .theme (m/ThemeData .primarySwatch m.Colors/pink))
   .home
   (m/Scaffold
    .appBar (m/AppBar
             .title (m/Text "ClojureDart Demo")))
   .body
   m/Center
   (m/Text "Hello World"
           .style (m/TextStyle
                   .color m.Colors/green
                   .fontSize 32.0))))
```

run project and watch changes to reload

- web

```sh
clj -M:cljd flutter -d web-server --web-port 8080
```
- android

```sh
clj -M:cljd flutter -d <device-id>
```
- desktop

```sh
clj -M:cljd flutter -d <windows/linux/macos>
```

# Tips

check alias `cljd` more functions

```sh
clj -M:cljd help
```

