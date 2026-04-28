func fib(_ n: Int) -> Int {
    if n<2 {
        return 1
    } else{
        return fib(n-1) + fib(n-2)
    }
}

let n = 40
print(fib(n))

