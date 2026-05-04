func fib(_ n: UInt64) -> UInt64 {
    if n < 2 {
        return 1
    } else{
        return fib(n-1) + fib(n-2)
    }
}

let n: UInt64 = 40
print(fib(n))
