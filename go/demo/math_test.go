package hello

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestMathDivMod(t *testing.T) {
	// truncate
	assert.Equal(t, 1, 3/2)
	assert.Equal(t, -1, -3/2)
	assert.Equal(t, 1, 3%2)
	assert.Equal(t, -1, -3%2)

	// floor

	// float
	assert.Equal(t, 1.5, float64(3)/2)
	assert.Equal(t, -1.5, float64(-3)/2)
}
