package main

import (
	"fmt"
	"time"
)

func main() {
	// test_chan()

	// test_map()

	// test_named_return()

	// test_destruct_slice()

	// test_escape_analyze()

	// test_zero_value()

	// test_time_it()
}

func test_chan() {
	ch := make(chan time.Time)
	go func(c chan<- time.Time) {
		v := time.Now()
		time.Sleep(1 * time.Second)
		c <- v
	}(ch)

	fmt.Println(time.Since(<-ch))
}

func put_if_absent[K comparable, V any](m map[K]V, k K, v V) {
	if _, ok := m[k]; !ok {
		m[k] = v
	}
}

func test_map() {
	// NOTE: gerneric not work for anonymous function
	// put_if_absent := func[K comparable, V any](m map[K]V, k K, v V) {
	// 	if _, ok := m[k]; !ok {
	// 		m[k] = v
	// 	}
	// }

	m := make(map[string]int)
	put_if_absent(m, "Go", 111)
	fmt.Println(m)
	put_if_absent(m, "Go", 222)
	fmt.Println(m)
}

func test_named_return() {
	host_port := func() (host string, port uint16) {
		host = "localhost"
		port = 9999
		return
	}

	host, port := host_port()
	println(host, port)
}

func test_destruct_slice() {
	a := []string{"Go", "Crystal"}
	b := []string{"Clojure", "Janet"}
	c := append(a, b...)
	fmt.Println(c)
}

func test_escape_analyze() {
	type Name struct {
		first string
		last  string
	}

	new_name := func(first, last string) *Name {
		n := Name{first, last}
		return &n
	}

	n := new_name("foo", "bar")
	fmt.Println(n)
}

func test_zero_value() {
	type HostPort struct {
		host string
		port uint16
	}

	host_port_on_heap := new(HostPort)

	var host_port_on_stack HostPort // same as	host_port_on_stack := HostPort{}

	fmt.Printf("%#v\n", host_port_on_heap)
	fmt.Printf("%#v\n", host_port_on_stack)
}

func test_time_it() {
	time_it := func(work func()) {
		start := time.Now()

		work()

		fmt.Println("time used:", time.Since(start))
	}

	time_it(func() {
		time.Sleep(1 * time.Second)

	})
}
