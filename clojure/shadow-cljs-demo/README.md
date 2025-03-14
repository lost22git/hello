# Shadow-cljs Demo

## Resources

- [Docs](https://shadow-cljs.github.io/docs/UsersGuide.html)

## Steps

### Create Project

```sh
npx create-cljs-project shadow-cljs-demo
```

### Configure `shadow-cljs.edn`

#### Add `cider/cider-nrepl`

```edn
:dependencies
;; shadow-cljs would add it as middleware into built-in nREPL server 
;; More info see: https://shadow-cljs.github.io/docs/UsersGuide.html#nREPL
 [[cider/cider-nrepl "0.52.0"]]
```
#### Add builds

```edn
;; Build configuration
;; More info see: https://shadow-cljs.github.io/docs/UsersGuide.html#_build_configuration
:builds
{;; Target: browser
 ;; More info see: https://shadow-cljs.github.io/docs/UsersGuide.html#target-browser
 :app {:target :browser
       :output-dir "public/app/js"
       :asset-path "/app/js"
       :modules {:main {:entries [lost.shadow-cljs-demo]}}}
...
}
```

#### Add dev-http (for Browser target)

```edn
:dev-http {8080 "public"} ;; dev http server for Browser target
```

### Start shadow-cljs server mode

Based on the configurations in `shadow-cljs.edn`,

it will

- Start a `Dashboard` http server
- Start a nREPL server
- Start a dev http server to serve your browser target app

```sh
shadow-cljs server
```
```
shadow-cljs - HTTP server available at http://localhost:8080
shadow-cljs - server version: 2.28.21 running at http://localhost:9630
shadow-cljs - nREPL server started on port 39592
```

OR 

```sh
shadow-cljs clj-repl
```
```
shadow-cljs - HTTP server available at http://localhost:8080
shadow-cljs - server version: 2.28.21 running at http://localhost:9630
shadow-cljs - nREPL server started on port 41926
shadow-cljs - REPL - see (help)
To quit, type: :repl/quit
shadow.user=>
```

### Watching and Compiling

```sh
shadow-cljs watch app
```

### Editor nREPL client connect to nREPL server

- `Conjure` in `Neovim`

```
:ConjureShadowSelect app
```


## Shadow-cljs, how to work?

```
                                        
                                | Shadow-cljs Server | ------read-----> | shadow-cljs.edn |
                                            |    |
                                          start  |------watching & compiling------> | compiled js | -------|
                                            |                                                              |
                                            V                                                              |
| nREPL Client | <------socket------> | nREPL Server | <------websocket------> | JS Runtime | <---run on---|
                                                                             
```


## Re-Frame

- [event-handling-infographic](https://day8.github.io/re-frame/event-handling-infographic/)
