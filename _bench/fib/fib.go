package main

import "fmt"

func fib(n uint64) uint64 {
	if n < 2 {
		return 1
	} else {
		return fib(n-1) + fib(n-2)
	}
}

func main() {
	var n uint64 = 40
	fmt.Println(fib(n))
}
