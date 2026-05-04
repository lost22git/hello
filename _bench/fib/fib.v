fn fib(n u64) u64 {
	if n < 2 {
		return 1
	} else {
		return fib(n - 1) + fib(n - 2)
	}
}

n := u64(40)
println(fib(n))
