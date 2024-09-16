package hello

import (
	// "fmt"
	"reflect"
	"testing"
	"unsafe"
	"encoding/binary"

	"github.com/stretchr/testify/assert"
)

func TestInt(t *testing.T) {
	assert.EqualValues(t, 8, unsafe.Sizeof(1))
	assert.EqualValues(t, 8, unsafe.Alignof(1))
	var i int
	assert.EqualValues(t, 8, unsafe.Sizeof(i))
	assert.EqualValues(t, 8, unsafe.Alignof(i))
	var i64 int64
	assert.EqualValues(t, 8, unsafe.Sizeof(i64))
	assert.EqualValues(t, 8, unsafe.Alignof(i64))
	var i32 int32
	assert.EqualValues(t, 4, unsafe.Sizeof(i32))
	assert.EqualValues(t, 4, unsafe.Alignof(i32))
	var i16 int16
	assert.EqualValues(t, 2, unsafe.Sizeof(i16))
	assert.EqualValues(t, 2, unsafe.Alignof(i16))
	var i8 int8
	assert.EqualValues(t, 1, unsafe.Sizeof(i8))
	assert.EqualValues(t, 1, unsafe.Alignof(i8))
	// ----------------------------------------
	var u uint
	assert.EqualValues(t, 8, unsafe.Sizeof(u))
	assert.EqualValues(t, 8, unsafe.Alignof(u))
	var u64 uint64
	assert.EqualValues(t, 8, unsafe.Sizeof(u64))
	assert.EqualValues(t, 8, unsafe.Alignof(u64))
	var u32 uint32
	assert.EqualValues(t, 4, unsafe.Sizeof(u32))
	assert.EqualValues(t, 4, unsafe.Alignof(u32))
	var u16 uint16
	assert.EqualValues(t, 2, unsafe.Sizeof(u16))
	assert.EqualValues(t, 2, unsafe.Alignof(u16))
	var u8 uint8
	assert.EqualValues(t, 1, unsafe.Sizeof(u8))
	assert.EqualValues(t, 1, unsafe.Alignof(u8))
	// ---------------------------------------
	var b byte
	assert.EqualValues(t, 1, unsafe.Sizeof(b))
	assert.EqualValues(t, 1, unsafe.Alignof(b))
	var r rune
	assert.EqualValues(t, 4, unsafe.Sizeof(r))
	assert.EqualValues(t, 4, unsafe.Alignof(r))
}

func TestFloat(t *testing.T) {
	assert.EqualValues(t, 8, unsafe.Sizeof(1.1))
	assert.EqualValues(t, 8, unsafe.Alignof(1.1))
	var d float64
	assert.EqualValues(t, 8, unsafe.Sizeof(d))
	assert.EqualValues(t, 8, unsafe.Alignof(d))
	var f float32
	assert.EqualValues(t, 4, unsafe.Sizeof(f))
	assert.EqualValues(t, 4, unsafe.Alignof(f))
}

func TestBool(t *testing.T) {
	assert.EqualValues(t, 1, unsafe.Sizeof(true))
	assert.EqualValues(t, 1, unsafe.Alignof(true))
}

func TestArray(t *testing.T) {
	assert.EqualValues(t, 1*4, unsafe.Sizeof([4]byte{0, 0, 0, 0}))
	assert.EqualValues(t, 1, unsafe.Alignof([4]byte{0, 0, 0, 0}))

	assert.EqualValues(t, 4*4, unsafe.Sizeof([4]int32{0, 0, 0, 0}))
	assert.EqualValues(t, 4, unsafe.Alignof([4]int32{0, 0, 0, 0}))
}

func TestString(t *testing.T) {
	assert.EqualValues(t, 16, unsafe.Sizeof("hello go"))
	assert.EqualValues(t, 16, unsafe.Sizeof("你好 go"))
	assert.EqualValues(t, 8, unsafe.Alignof("你好 go"))
}

func TestSlice(t *testing.T) {
	var slice = make([]int, 4)
	assert.EqualValues(t, 8+8+8, unsafe.Sizeof(slice))
	assert.EqualValues(t, 8, unsafe.Alignof(slice))

	var sliceHeader = *(*reflect.SliceHeader)(unsafe.Pointer(&slice))
	assert.Equal(t, len(slice), sliceHeader.Len)
	assert.Equal(t, cap(slice), sliceHeader.Cap)
	assert.EqualValues(t, 0, unsafe.Offsetof(sliceHeader.Data))
	assert.EqualValues(t, 8, unsafe.Offsetof(sliceHeader.Len))
	assert.EqualValues(t, 16, unsafe.Offsetof(sliceHeader.Cap))
}

type Account struct {
	Id      uint32
	Name    string
	Balance uint32
}

type Account2 struct {
  Name    string
	Id      uint32
	Balance uint32
}

func TestStrcut(t *testing.T) {
	var a = Account{}
	assert.EqualValues(t, 8+16+8, unsafe.Sizeof(a))
	assert.EqualValues(t, 8, unsafe.Alignof(a))
	assert.EqualValues(t, 0, unsafe.Offsetof(a.Id))
	assert.EqualValues(t, 8, unsafe.Offsetof(a.Name))
	assert.EqualValues(t, 24, unsafe.Offsetof(a.Balance))

	var a2 = Account2{}
	assert.EqualValues(t, 16+4+4, unsafe.Sizeof(a2))
	assert.EqualValues(t, 8, unsafe.Alignof(a2))
  assert.EqualValues(t, 0, unsafe.Offsetof(a2.Name))
	assert.EqualValues(t, 16, unsafe.Offsetof(a2.Id))
	assert.EqualValues(t, 20, unsafe.Offsetof(a2.Balance))
}

func TestFunction(t *testing.T) {
	var f = func(x, y int) int {
		return x + y
	}
	assert.EqualValues(t, 8, unsafe.Sizeof(f))
	assert.EqualValues(t, 8, unsafe.Alignof(f))
}

func TestClosure(t *testing.T) {
	var a int = 11
	var f = func(b int) int {
		return a + b
	}
	assert.Equal(t, f(11), 22)
	assert.EqualValues(t, 8, unsafe.Sizeof(f))
	assert.EqualValues(t, 8, unsafe.Alignof(f))
}

func TestInterface(t *testing.T) {
	var a interface{}
	assert.EqualValues(t, 8+8, unsafe.Sizeof(a))
	assert.EqualValues(t, 8, unsafe.Alignof(a))
}

func TestChannel(t *testing.T) {
	var a = make(chan int, 10)
	assert.EqualValues(t, 8, unsafe.Sizeof(a))
	assert.EqualValues(t, 8, unsafe.Alignof(a))
}

func TestMap(t *testing.T) {
	var a = make(map[string]string)
	assert.EqualValues(t, 8, unsafe.Sizeof(a))
	assert.EqualValues(t, 8, unsafe.Alignof(a))
}


func TestEndian(t *testing.T) {
	bs := []byte{0, 0, 0, 0}
	data := uint32(257)

	// BigEndian encode
	binary.BigEndian.PutUint32(bs, data)
	assert.Equal(t, []byte{0, 0, 1, 1}, bs)
	// BigEndian decode
	assert.Equal(t, binary.BigEndian.Uint32(bs), data)

	// LittleEndian encode
	binary.LittleEndian.PutUint32(bs, data)
	assert.Equal(t, []byte{1, 1, 0, 0}, bs)
	// LittleEndian decode
	assert.Equal(t, binary.LittleEndian.Uint32(bs), data)
}
