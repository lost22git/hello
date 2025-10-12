package main

import (
	"fmt"
)

func main() {
	n := 111
	fmt.Println("fib_iterate", n, "=", fib_iterate(n))
	fmt.Println("fib_tail_recur", n, "=", fib_tail_recur(n))
	fmt.Println("fib_recur", n, "=", fib_recur(n))
}

func fib_iterate(n int) int {
	a, b := 1, 1
	for range n - 1 {
		a, b = a+b, a
	}
	return a
}

func fib_recur(n int) int {
	if n <= 1 {
		return 1
	}
	return fib_recur(n-1) + fib_recur(n-2)
}

func fib_tail_recur(n int) int {
	return visit(n, 1, 0)
}

func visit(i int, a int, b int) int {
	if i == 0 {
		return a
	}
	return visit(i-1, a+b, a)
}
