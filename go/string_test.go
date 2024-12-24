package hello

import (
	"fmt"
	"strings"
	"testing"
	"unicode/utf8"

	"github.com/stretchr/testify/assert"
)

func TestStringLen(t *testing.T) {
	assert.Equal(t, 5, len("hello"))
	assert.Equal(t, 2*3, len("你好"))
	assert.Equal(t, 5, utf8.RuneCountInString("你好 go"))
}

func TestStringMultiline(t *testing.T) {
	a := `
{
  "name": "go"
}
`
	assert.Equal(t, "\n{\n  \"name\": \"go\"\n}\n", a)
}

func TestStringConcat(t *testing.T) {
	assert.Equal(t, "hello go", "hello "+"go")
}

func TestStringFormat(t *testing.T) {
	assert.Equal(t, "hello go", fmt.Sprintf("hello %v", "go"))
}

func TestStringTrim(t *testing.T) {
	assert.Equal(t, "hello", strings.TrimSpace("  hello "))
	assert.Equal(t, "go", strings.TrimPrefix("hello go", "hello "))
	assert.Equal(t, "hello", strings.TrimSuffix("hello go", " go"))
	assert.Equal(t, "Go", strings.TrimLeft("gggGo", "g"))
	assert.Equal(t, "o", strings.TrimLeft("gggGo", "Gg"))
	assert.Equal(t, "gggG", strings.TrimRight("gggGo", "aoe"))
}

func TestStringCut(t *testing.T) {
	assert.Equal(t, MultiValuesToSlice("你", "go", true), MultiValuesToSlice(strings.Cut("你好go", "好")))

	assert.Equal(t, MultiValuesToSlice("你好go", false), MultiValuesToSlice(strings.CutPrefix("你好go", "好")))
	assert.Equal(t, MultiValuesToSlice("go", true), MultiValuesToSlice(strings.CutPrefix("你好go", "你好")))

	assert.Equal(t, MultiValuesToSlice("你好go", false), MultiValuesToSlice(strings.CutSuffix("你好go", "好")))
	assert.Equal(t, MultiValuesToSlice("你好", true), MultiValuesToSlice(strings.CutSuffix("你好go", "go")))
}

func TestStringReplace(t *testing.T) {
	assert.Equal(t, "你好 go", strings.ReplaceAll("hello go", "hello", "你好"))
}

func TestStringSplit(t *testing.T) {
	assert.Equal(t, []string{"hello", "", "go", "lang"}, strings.Split("hello::go:lang", ":"))
	assert.Equal(t, []string{"hello", ":go:lang"}, strings.SplitN("hello::go:lang", ":", 2))
}

func TestStringJoin(t *testing.T) {
	list := []string{
		"hello",
		"go",
		"lang",
	}
	assert.Equal(t, "hello-go-lang", strings.Join(list, "-"))
}

func TestStringSubString(t *testing.T) {
	assert.Equal(t, "go", "hello go"[6:8])
	assert.Equal(t, "go", "hello go"[6:])
	assert.Equal(t, "hello", "hello go"[:5])
	assert.Equal(t, "hello go", "hello go"[:])
}

func TestStringBuilder(t *testing.T) {
	var sb strings.Builder
	sb.WriteString("hello")
	sb.WriteByte(byte(' '))
	sb.Write([]byte{'g', 'o'})
	assert.Equal(t, "hello go", sb.String())
	assert.Equal(t, 8, sb.Len())
	assert.Equal(t, 8, sb.Cap())
}

func TestStringContains(t *testing.T) {
	assert.True(t, strings.Contains("你好 go", "go"))
	assert.True(t, strings.Contains("你好 go", "你好"))
}

func TestStringStartsWith(t *testing.T) {
	assert.True(t, strings.HasPrefix("你好go", "你好"))
}

func TestStringEndsWith(t *testing.T) {
	assert.True(t, strings.HasSuffix("你好go", "go"))
}

func TestStringRepeat(t *testing.T) {
	assert.Equal(t, "你好你好你好", strings.Repeat("你好", 3))
}
