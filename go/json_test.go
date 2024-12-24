package hello

import (
	"encoding/json"
	"testing"

	"github.com/stretchr/testify/assert"
)

type Book struct {
	Id         uint     `json:"id"`
	Name       string   `json:"name"`
	Categories []string `json:"categories"`
	Price      float64  `json:"price,omitempty"`
}

func TestJsonEncode(t *testing.T) {
	book := Book{
		Id:         11,
		Name:       "《史记》",
		Categories: []string{"历史", "中国"},
		Price:      0.0,
	}
	if jsonBytes, err := json.MarshalIndent(book, "", "    "); err != nil {
		assert.Failf(t, "Failed to json encode", "book=%+v err=%v", book, err)
	} else {
		want := `{
    "id": 11,
    "name": "《史记》",
    "categories": [
        "历史",
        "中国"
    ]
}`
		assert.Equal(t, want, string(jsonBytes))
	}
}

func TestJsonDecode(t *testing.T) {
	jsonBytes := []byte(`
{
    "id": 11,
    "name": "《史记》",
    "categories": [
      "历史", 
      "中国"
    ]
}
`)
	var book Book
	if err := json.Unmarshal(jsonBytes, &book); err != nil {
		assert.Failf(t, "Failed to json decode", "json=%q err=%v", string(jsonBytes), err)
	}
	want := Book{Id: 11, Name: "《史记》", Categories: []string{"历史", "中国"}}
	assert.Equal(t, want, book)
}
