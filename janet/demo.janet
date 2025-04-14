#!/usr/bin/env janet

(comment
  # === Doc ===

  (doc pp) # find symbol `pp`
  (print (string/repeat "=" 22))
  (doc "dir") # fuzzy find symbol matched "dir"
  (print (string/repeat "=" 22))

  # end comment
)

(comment
  # === Print ===

  (tracev (+ 1 1)) # for debug
  (pp {:a 1 :b 2}) # prettyprint a value
  (print "I'm printed to (or (dyn *out*) stdout)")
  (xprint (or (dyn *out*) stdout) "I'm printed to (or (dyn *out*) stdout)")
  (eprint "I'm printed to (or (dyn *err*) stderr)")
  (xprint (or (dyn *err*) stderr) "I'm printed to (or (dyn *err*) stderr)")
  (printf "printf %+10s : %010d" "Janet" 11)

  (let [buf @""]
    (with-dyns [*out* buf]
      (print "ssss"))
    (assert (deep= (string buf)
                   "ssss\n")))

  # end comment
)

(comment
  # === String ===

  # multi-lines raw string (text block)
  (print ``
# Languages
  - `Janet`
  - `Fennel`
``)

  # end comment
)

(comment
  # === Array/Tuple ===
  # array (mutable) => @[]
  # tuple (immutable) => []

  (-> ["Janet" "Fennel"]
      type)

  (-> @["Janet"]
      (array/push "Fennel")
      type)

  # end comment
)

(comment
  # === Table/Struct ===
  # table (mutable) => @{}
  # struct (immutable) => {}

  (-> {:name "Janet" :family :Lisp}
      type)

  (-> @{:name "janet" :family :Lisp}
      (put :name "Janet")
      type)

  # end comment
)


(comment
  (try
    (do
      (def n (getline "INPUT A NUMBER: "))
      (print "YOUR NUMBER IS: " (int/s64 (string/trim n))))
    ([e] (eprint "ERROR: " e))))

(comment
  # === Error ===

  (protect
    10)

  (protect
    (error -1)
    10)

  (try (error -1)
    ([err fiber]
      (debug/stacktrace fiber err "")
      0))

  # end comment
)

(comment
  # === For/Loop ===

  # for i [1,5) do-effect
  (for i 1 5
    (print i))

  # loop bindings do-effect
  (loop [i :range [1 5 1]]
    (print i))
  (loop [i :in (range 1 5 1)]
    (print i))
  (each i (range 1 5 1)
    (print i))

  # end comment
)


(comment
  # === Function ===
  # https://janet-lang.org/docs/functions.html

  (defn hello
    "Hello to given name"
    [name]
    (print "Hello " name " !!"))

  (def name :private "The name of Janet" "Janet")
  (hello name)

  # lambda |()
  (->> ["Janet" "Fennel"]
       (map |(string/ascii-upper $)))

  # fib
  (defn fib [n]
    (case n
      0 1
      1 1
      (+ (fib (- n 1)) (fib (- n 2)))))

  (print "fib(11): " (fib 11))

  # &keys params
  (defn add-keys [&keys {:a a :b b}]
    (+ a b))

  (add-keys :a 1 :b 2)
  (add-keys ;(kvs {:a 1 :b 2}))
  (apply add-keys (kvs {:a 1 :b 2}))

  # &named params
  (defn add-named [&named a b]
    (+ a b))

  (add-named :a 1 :b 2)
  (add-named ;(kvs {:a 1 :b 2}))
  (apply add-named (kvs {:a 1 :b 2}))

  # end comment
)

(comment
  # === Buffer ===

  (-> @""
      (buffer/push-byte 97)
      (buffer/push-string "-")
      (buffer/push-byte 98)
      (string))

  (def buf (-> @""
               (buffer/push-uint32 :le 11)
               (buffer/push-uint32 :le 22)
               (buffer/push-uint32 :be 33)))

  (assert
    (deep=
      (peg/match '(uint 4) buf)
      @[11]))
  (assert
    (deep=
      (peg/match '(uint 4) buf 4)
      @[22]))
  (assert
    (deep=
      (peg/match '(uint-be 4) buf 8)
      @[33]))

  # end comment
)

(comment
  # === PEG ===
  # https://janet-lang.org/docs/peg.html
  # - primitive patterns
  # - combining patterns
  # - captures
  # - recursion

  (def hexcolor
    ~{:hex (range "09" "af" "AF") # :h
      :hexhex (2 :hex)
      :capture-hexhex (<- :hexhex)
      :capture-hexhex-with-pos (*
                                 ($)
                                 :capture-hexhex
                                 ($))
      :main (*
              "#"
              (3 :capture-hexhex-with-pos))})

  (peg/match hexcolor "#FF800F")

  # end comment
)


(comment
  # === Fiber ===

  (let [f (fiber/current)]
    (print "=== fiber current ===")
    (pp (fiber/status f))
    (pp (fiber/getenv f))
    (pp (fiber/maxstack f))
    (pp (fiber/last-value f)))

  (let [f (fiber/root)]
    (print "=== fiber root ===")
    (pp (fiber/status f))
    (pp (fiber/getenv f))
    (pp (fiber/maxstack f))
    (pp (fiber/last-value f)))

  (let [ch (ev/chan)
        supervisor (ev/chan)
        n 4
        giver-fib (ev/spawn (do (repeat n
                                  (ev/sleep 1)
                                  (ev/give ch (os/time)))
                              (ev/chan-close ch)))
        taker-fib (ev/go |(forever
                            (if-let [v (ev/take $)]
                              (print v) (break))) ch supervisor)]

    (pp (ev/take supervisor))
    (ev/chan-close supervisor)
    (print "taker-fib" taker-fib ": " (fiber/status taker-fib))
    (print "giver-fib" giver-fib ": " (fiber/status giver-fib)))

  (try
    (ev/with-deadline 3
      (do
        (ev/sleep 1)
        (print "first sleep done")
        (ev/sleep 1)
        (print "second sleep done")
        (ev/sleep 1)
        (print "third sleep done")))
    ([e f]
      (print "last-value: " (fiber/last-value f))
      (debug/stacktrace f e "")
      nil))

  (let [generator
        (coro (yield 1) (yield 10) (yield 100))]
    (each v generator
      (print v)))

  # end comment
)

(comment
  # === Thread ===

  (ev/do-thread
    (print "hello from do-thread")) # ? how to get thread id or fiber id

  (ev/spawn-thread
    (print "hello from spawn-thread"))

  # end comment
)


(comment
  # === OS ===

  (os/arch)
  (os/which)
  (os/compiler)
  (os/cpu-count)
  (os/cryptorand 10)
  # time
  (os/clock)
  (os/clock :realtime :tuple)
  (os/clock :cputime :tuple)
  (os/clock :monotonic :tuple)
  (os/date)
  (os/time)
  (->> (os/dir ".")
       (map |(string (os/cwd) "/" $)))
  # env
  (os/getenv "TMPDIR")
  (each [k v] (pairs (os/environ))
    (print k "=>" v))
  # file
  (os/stat "janet")
  (let [stat (os/lstat "janet")
        p-int (get stat :int-permissions)
        p-str (get stat :permissions)]
    (assert (= p-int (os/perm-int p-str)))
    (assert (= p-str (os/perm-string p-int))))
  (defer (os/rm "test-new-dir")
    (assert (os/mkdir "test-new-dir"))
    (-?> (os/stat "test-new-dir")
         (get :mode)
         (= :directory)
         assert))

  # process & shell
  # inherit :in/:out/:err of current process
  # just return exit-code
  (os/shell "janet -v")
  (os/execute ["janet" "-v"] :p)
  # os/execute :out tempfile
  (try
    (with [f (file/temp)]
      (def cmd ["janet" "-v"])
      (os/execute cmd :px {:out f})
      (file/seek f :set 0)
      (string/trim (file/read f :all)))
    ([e] (eprint e)))
  # os/spawn :out tempfile
  (try
    (with [f (file/temp)]
      (let [cmd ["janet" "-v"]
            p (os/spawn cmd :px {:out f})
            exit-code (os/proc-wait p)]
        (file/seek f :set 0)
        (string/trim (file/read f :all))))
    ([e] (eprint e)))
  # os/spawn :out :pipe
  # interact with :in/:out
  (try
    (let [result @""
          cmd ["janet" "-v"]
          p (os/spawn cmd :px {:out :pipe})]
      (ev/gather
        (ev/read (p :out) :all result)
        (os/proc-wait p))
      (string/trim result))
    ([e] (eprint e)))

  # end comment
)


(comment
  # === Net ===

  # end comment
)


(comment
  # === FFI ===

  # end comment
)

(comment
  # === JSON ===

  (import spork/json)

  (let [buf @""
        data {:name "Janet"
              :tags @["Functional" "Lisp"]}]
    (json/encode data "" "" buf)
    (-> (json/decode buf true true)
        (table/to-struct)
        tracev
        (deep= data)
        assert))

  # end comment
)


(comment
  # ===  ArgParse ===

  (import spork/argparse :prefix "")
  (import spork/schema)

  (def cli-spec ["Greeting cli app"
                 "name" {:help "Name to greet"
                         :kind :option
                         :required true}
                 "times" {:help "Times of greeting"
                          :short "n"
                          :kind :option
                          :required false
                          :default "1"
                          :map scan-number}])

  (def cli-validator
    (schema/validator
      (props
        "name" (and :string (length 3 10))
        "times" (and :number (pred pos?)))))

  (with-dyns [:args
              @["app" "--name" "king" "-n" "-2"]]
    (let [{"name" name "times" times}
          (cli-validator (argparse ;cli-spec))]
      (repeat times
        (print "HELLO -> " name))))

  # end comment
)


(comment
  # === HTTP ===

  (import spork/http)

  (def resp (http/request
              "GET"
              "http://api64.ipify.org"
              :headers
              {"User-Agent" "curl/8.12.1"
               "Accept" "*/*"}))

  (case (resp :status)
    200
    (-> resp
        (get :body)
        pp)
    (-> resp
        (get :message)
        pp))

  # end comment
)
