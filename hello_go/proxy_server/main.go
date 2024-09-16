package main

import (
	"fmt"
	"net/http"
	"time"
)

type ProxyHandler struct {
	Client *http.Client
}

func (h *ProxyHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	uri := "https://httpbin.org" + r.URL.Path
	if request, err := http.NewRequest(r.Method, uri, r.Body); err != nil {
		panic(err)
	} else {
		request.Header.Set("Host", "httpbin.org")
		fmt.Printf("Do request: [%s] %s\n", r.Method, uri)
		if response, err := h.Client.Do(request); err != nil {
			panic(err)
		} else {
			response.Write(w)
		}
	}
}

func StartServer() {
	proxyHandler := new(ProxyHandler)
	proxyHandler.Client = &http.Client{
		Transport: &http.Transport{Proxy: http.ProxyFromEnvironment},
		Timeout:   10 * time.Second,
	}
	http.Handle("GET /", proxyHandler)
	fmt.Println("Proxy server is running on :8000")
	if err := http.ListenAndServe(":8000", nil); err != nil {
		panic(err)
	}
}

func main() {
	StartServer()
}
