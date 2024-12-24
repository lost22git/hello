module [
    Lazy,
    tryInit,
    get,
]

Lazy a : [UnInited ({} -> a), Inited a]

tryInit : Lazy a -> Lazy a
tryInit = \lz ->
    when lz is
        UnInited fn -> Inited (fn {})
        Inited _ -> lz

get : Lazy a -> Result a [LazyUnInitedErr (Lazy a)]
get = \lz ->
    when lz is
        UnInited _ -> Err (LazyUnInitedErr lz)
        Inited v -> Ok v
