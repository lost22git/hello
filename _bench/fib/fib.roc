app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br" }

import pf.Stdout

main! = |_args|
    n = 40
    Stdout.line! (Num.to_str (fib n))

fib = |n|
    if n < 2 then
        1
    else
        fib (n - 1) + fib(n - 2)
