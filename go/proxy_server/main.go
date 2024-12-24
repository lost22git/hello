package main

import (
	"errors"
	"io"
	"log/slog"
	"net/http"
	"os"
	"time"
)

func setupLogger() {
	logLevel := &slog.LevelVar{}
	logLevel.Set(slog.LevelDebug)
	handler := slog.NewTextHandler(
		os.Stdout,
		&slog.HandlerOptions{AddSource: false, Level: logLevel},
	)
	logger := slog.New(handler)
	slog.SetDefault(logger)
}

func main() {
	setupLogger()
	startServer()
}

func startServer() {
	proxyHandler := new(ProxyHandler)
	proxyHandler.Client = &http.Client{
		Transport: &http.Transport{Proxy: http.ProxyFromEnvironment},
		Timeout:   10 * time.Second,
	}
	http.Handle("GET /", proxyHandler)
	http.Handle("POST /", proxyHandler)
	slog.Info("Proxy server is running on :8000")
	if err := http.ListenAndServe(":8000", nil); err != nil {
		panic(err)
	}
}

type ProxyHandler struct {
	Client *http.Client
}

func headerCopyFrom(dst http.Header, src http.Header) {
	for k, v := range src {
		for _, s := range v {
			dst.Add(k, s)
		}
	}
}

func (h *ProxyHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	target := r.URL.Query().Get("target")
	if target == "" {
		panic(errors.New("Param not found: target"))
	}
	targetReq, err := http.NewRequest(r.Method, target, r.Body)
	if err != nil {
		panic(err)
	}
	slog.Info("Fetching", "method", targetReq.Method, "target", targetReq.URL)
	targetRes, err := h.Client.Do(targetReq)
	if err != nil {
		panic(err)
	}
	defer targetRes.Body.Close()
	headerCopyFrom(w.Header(), targetRes.Header)
	w.WriteHeader(targetRes.StatusCode)
	if _, err := io.Copy(w, targetRes.Body); err != nil {
		panic(err)
	}
}
