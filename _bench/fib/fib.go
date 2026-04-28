package main

import "fmt"

func fib(n int) int {
	if n < 2 {
		return 1
	} else {
		return fib(n-1) + fib(n-2)
	}
}

func main() {
	n := 40
	fmt.Println(fib(n))
}
