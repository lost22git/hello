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

  (try
    (do
      (def n (getline "INPUT A NUMBER: "))
      (print "YOUR NUMBER IS: " (int/s64 (string/trim n))))
    ([e] (eprint "ERROR: " e)))

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
