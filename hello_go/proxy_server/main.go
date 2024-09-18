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
	handler := slog.NewTextHandler(os.Stdout, &slog.HandlerOptions{AddSource: false, Level: logLevel})
	logger := slog.New(handler)
	slog.SetDefault(logger)
}

func main() {
	setupLogger()
	StartServer()
}

func StartServer() {
	proxyHandler := new(ProxyHandler)
	proxyHandler.Client = &http.Client{
		Transport: &http.Transport{Proxy: http.ProxyFromEnvironment},
		Timeout:   10 * time.Second,
	}
	http.Handle("GET /", proxyHandler)
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
			dst.Set(k, s)
		}
	}
}

func (h *ProxyHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if target := r.URL.Query().Get("target"); target != "" {
		if targetReq, err := http.NewRequest(r.Method, target, r.Body); err == nil {
			slog.Info("Do request", "method", targetReq.Method, "target", targetReq.URL)
			if targetRes, err := h.Client.Do(targetReq); err == nil {
				headerCopyFrom(w.Header(), targetRes.Header)
				w.WriteHeader(targetRes.StatusCode)
				if _, err := io.Copy(w, targetRes.Body); err != nil {
					panic(err)
				}
				defer targetRes.Body.Close()
			} else {
				panic(err)
			}
		} else {
			panic(err)
		}
	} else {
		panic(errors.New("Param not found: target"))
	}
}
