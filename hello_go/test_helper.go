package hello

import (
	"fmt"
)

func MultiValuesToSlice(a ...interface{}) []interface{} {
	return a
}

func PrintKeyValue(key string, value interface{}) {
	fmt.Printf("%-15s : %v\n", key, value)
}
