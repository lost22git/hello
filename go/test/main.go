package main

import (
	"errors"
	"fmt"
	"reflect"
	"slices"
	"time"
	"unsafe"
)

func main() {
	// test_array()

	// test_slice()

	// test_chan()

	// test_map()

	// test_named_return()

	// test_destruct_slice()

	// test_escape_analyze()

	// test_zero_value()

	// test_time_it()

	// test_multi_values()

	// test_reflect_tag_of_field()

	// test_errors()

	// test_iterator()

	// test_methods()

}

func test_array() {
	a := [2]string{"foo", "bar"}
	b := [...]string{"foo", "bar"}
	fmt.Printf("a: %[1]T %[1]v\n", a)
	fmt.Printf("b: %[1]T %[1]v\n", b)
	fmt.Println("a==b:", a == b)
}

func test_slice() {
	v := "foo"
	a := []string{v, "goo"}
	b := append(a, "bar")
	c := append(b, "zar")
	d := append(c, "koo")
	fmt.Println("a:", len(a), cap(a), a, &a[0])
	fmt.Println("b:", len(b), cap(b), b, &b[0])
	fmt.Println("c:", len(c), cap(c), c, &c[0])
	fmt.Println("d:", len(d), cap(d), d, &d[0])
	e := slices.Delete(d, len(d)-2, len(d)-1) // delete "zar"
	fmt.Println("e:", len(e), cap(e), e, &e[0])
	fmt.Println("d:", len(d), cap(d), d, &d[0]) // "zar" <- "koo", "koo" <- ""

	f := e[0:2]
	fmt.Println("f:", len(f), cap(f), f, &f[0])

	g := append(f, "boo")
	fmt.Println("g:", len(g), cap(g), g, &g[0])
	fmt.Println("f:", len(f), cap(f), f, &f[0])
	fmt.Println("e:", len(e), cap(e), e, &e[0])

	// specify len and cap
	n := make([]int, 0, 10)
	fmt.Println("n:", len(n), cap(n), n)
	m := make([]int, 2, 10)
	fmt.Println("m:", len(m), cap(m), m)

	// empty slice
	empty_slice := []string{}
	fmt.Println("empty_slice:", len(empty_slice), cap(empty_slice), empty_slice)
	fmt.Println("empty_slice sizeof:", unsafe.Sizeof(empty_slice))
	fmt.Println("empty_slice header:", *(*reflect.SliceHeader)(unsafe.Pointer(&empty_slice)))

	// nil slice
	var nil_slice []string = nil
	fmt.Println("nil_slice:", len(nil_slice), cap(nil_slice), nil_slice)
	fmt.Println("nil_slice sizeof:", unsafe.Sizeof(nil_slice))
	fmt.Println("nil slice header:", *(*reflect.SliceHeader)(unsafe.Pointer(&nil_slice)))
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

func test_multi_values() {
	get_host_port := func() (host string, port uint16) {
		return "localhost", 9999
	}
	fmt_host_port := func(host string, port uint16) string {
		return fmt.Sprintf("%s:%d", host, port)
	}
	fmt.Println(fmt_host_port(get_host_port()))
}

func test_reflect_tag_of_field() {
	type HostPort struct {
		host string `note:"hostname or ip"`
		port uint16 `note:"port range from 1 to 65535"`
	}

	host_port := HostPort{}
	f_host, _ := reflect.TypeOf(host_port).FieldByName("host")
	println(f_host.Tag.Get("note"))
}

type myError struct {
	msg string
}

var myErr = myError{msg: "oops my error!!!"}

func (err myError) Error() string {
	return fmt.Sprintf("[%T]: %s", err, err.msg)
}

func raise_myerror() error {
	return myErr
}

func wrap_myerror() error {
	return fmt.Errorf("wrapper error: %w", raise_myerror())
}

func test_errors() {
	err := wrap_myerror()
	fmt.Println(err)
	if errors.Is(err, myErr) {
		println("wrapper of myErr")
		var my_err myError
		if errors.As(err, &my_err) {
			println(my_err.msg)
		}
	} else {
		println("not wrapper of myErr")
	}
}

func test_iterator() {
	a := []string{"foo", "bar", "koo"}
	a_iter := func(consume func(i int, v string) bool) {
		for i, v := range a {
			if !consume(i, v) {
				return
			}
		}
	}

	for i, v := range a_iter {
		println(i, v)
	}

	println("--- simulate for range iterator ---")

	a_iter(func(i int, v string) bool {
		println(i, v)
		return true
	})

	println("--- for range a integer ---")
	for v := range 10 {
		println(v)
	}

}

type Counter struct {
	value uint32
}

// immutable receiver: passed by copy or by ref
func (c Counter) Value() uint32 {
	return c.value
}

// mutable receiver: passed by ref
func (c *Counter) Inc(delta uint32) {
	c.value += delta
}

func test_methods() {
	c := Counter{}
	c.Inc(10)
	println(c.Value())

	hc := &Counter{}
	hc.Inc(10)
	println(hc.Value())
}
