#!/usr/bin/env lfe


(c "foo.lfe")

(foo:double 10)

(foo:fib 11)
(foo:fib "11")


; === tuple ===

; tuple literal
#("i" "am" "a" "tuple")

; tuple runtime ctor
(tuple "i" "am" "a" "tuple")


; === list ===

; list literal
(1 2 3)

[1 2 3]

; list runtime ctor
(set n 1)
(list n 2 3)


; === propertylist ===

(proplists:get_value 'a (list (tuple 'a 1) (tuple 'b 2)))

(proplists:get_value 'c '(#(a 1) #(b 2)) -1)


; === map ===

(mref #m("name" "lfe") "name")
(map-get #m("name" "lfe") "name")

(maps:get "name" (map "name" "lfe"))
(maps:get "age" #m("name" "lfe") (- 2025 2007))


; === def ===

(set first 1)
first
; def pattern-matching
(set (cons first rest) '("erlang" "lfe" "elixir"))
first


; === let-bindings ===

(let [(a 1)
      (b 2)]
  (+ a b))


; let-bindings pattern-matching
(let [((tuple a 2) #(1 2))]
  (+ a 2))


; === print to stdout ===

(lfe_io:format "lfe_io: hello, ~s~n" '[LFE])
(io:format "io: hello, ~s~n" '[LFE])
(io:format "io: hello, ~p~n" '[LFE])

(p "LFE")
(pp "LFE")
(ep "LFE")
(epp "LFE")


; === if ===

(if (== 1 1)
  "EQ"
  "NE")


; === cond ===

(set n 2)
(cond 
((== 1 n) "EQ")
((<  1 n) "GT")
('true "LT"))


; === case ===

(case n
  (0 0)
  (1 1)
  (_ 2))



; === record ===

(defrecord book
  (title (string))
  (tag '[] (list)))

(size-book)
(fields-book) ; list all fields of book

; create
(set b (make-book title "lfe tutorial"))

; check
(is-book b)

; get
(book-title b)
(book-title) ; return index of title in book
(book-tag)

; update
(update-book-title b "LFE TUTORIAL")
(update-book b tag '[fle-lang])

; match
(case b
  ((match-book title a tag b) 
   (lfe_io:format "match-book: title:~p tag:~p~n" (list a b)))
  (_ (lfe_io:format "match-book: not match" '())))


; === try ===

(try
  (list_to_integer "pi")
  (catch
    ((tuple type value stacktrace) 
     (lfe_io:format "catch a error: type:~p value:~p~nstacktrace:~p~n" (list type value stacktrace))
     -1)
    (_ -2))
  (after 
    (pp "running after block in try block")))


; === clojure ===

(clj:-> #m(a 1)
  (map-get 'a))

(clj:defn add [a b] (+ a b))
(add 1 2)

(clj:str "I am" " a " "string")


; === process ===

(register 'fib-server 
  (spawn 'foo 'fib-handler ()))

(set fiber-server-pid (whereis 'fib-server))

(lfe_io:format "fib-server is serving.., pid: ~p~n" (list fiber-server-pid))

(register 'fib-client 
  (spawn 'foo 'fib-request (list fiber-server-pid)))

(timer:sleep 3000)

