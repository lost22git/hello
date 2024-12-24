package hello

import (
	"slices"
	"testing"
	"unsafe"

	"github.com/stretchr/testify/assert"
)

/// go lang 的 slice 相比其他語言的 slice 多一個 `Cap` 字段,
/// 因而具有动态length的特性
///

func TestSliceUnsafeSlice(t *testing.T) {
	a := [4]string{"hello", "go", "你好", "世界"}
	// same as s = a[:2]
	s := unsafe.Slice(&a[0], 2)
	assert.Equal(t, []string{"hello", "go"}, s)
}

func TestSliceMake(t *testing.T) {
	s := make([]string, 4)
	assert.Equal(t, []string{"", "", "", ""}, s)
}

func TestSliceAppend(t *testing.T) {
	s := []string{"hello", "go"}
	ss := append(s, "你好", "世界")
	assert.Equal(t, []string{"hello", "go"}, s)
	assert.Equal(t, []string{"hello", "go", "你好", "世界"}, ss)
}

func TestSliceConcat(t *testing.T) {
	a := []string{"hello", "go"}
	b := []string{"你好", "世界"}
	c := slices.Concat(a, b)
	assert.Equal(t, []string{"hello", "go", "你好", "世界"}, c)
}

func TestSliceDelete(t *testing.T) {
	s := []string{"hello", "go", "你好", "世界"}
	s = slices.Delete(s, 0, 2)
	assert.Equal(t, []string{"你好", "世界"}, s)
}

func TestSliceCopy(t *testing.T) {
	src := []string{"hello", "go"}
	dst := []string{"", "", "你好", "世界"}
	copy(dst[:len(src)], src)
	assert.Equal(t, []string{"hello", "go", "你好", "世界"}, dst)
}
