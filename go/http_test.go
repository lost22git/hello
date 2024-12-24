package hello

import (
	"bytes"
	"encoding/json"
	"net/http"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestHttpGet(t *testing.T) {
	client := &http.Client{Timeout: 2 * time.Second}
	if request, err := http.NewRequest("GET", "https://httpbin.org/status/800", bytes.NewReader([]byte(""))); err != nil {
		assert.Failf(t, "Failed to new http request", "err=%v", err)
	} else {
		if response, err := client.Do(request); err != nil {
			assert.Failf(t, "Failed to http get", "err=%v", err)
		} else {
			assert.Equal(t, 800, response.StatusCode)
		}
	}
}

func TestHttpPost(t *testing.T) {
	client := &http.Client{Timeout: 2 * time.Second}
	if request, err := http.NewRequest("POST", "https://httpbin.org/post", bytes.NewReader([]byte(`{"msg":"hello go"}`))); err != nil {
		assert.Failf(t, "Failed to new http request", "err=%v", err)
	} else {
		request.Header.Add("Content-Type", "application/json")

		if response, err := client.Do(request); err != nil {
			assert.Failf(t, "Failed to http post", "err=%v", err)
		} else {
			defer response.Body.Close()
			var dummy struct {
				Data string `json:"data"`
			}
			if err := json.NewDecoder(response.Body).Decode(&dummy); err != nil {
				assert.Failf(t, "Failed to json decode response body", "err=%v", err)
			}
			assert.Equal(t, `{"msg":"hello go"}`, dummy.Data)
		}
	}
}
