package hello

import (
	"testing"
	"time"
	// "github.com/stretchr/testify/assert"
)

func TestTimeFormat(t *testing.T) {
	now := time.Now()
	PrintKeyValue("now", now)
	PrintKeyValue("now utc", now.UTC())
	PrintKeyValue("now unix", now.Unix())
	PrintKeyValue("now fmt RFC3339", now.Format(time.RFC3339))
	PrintKeyValue("now fmt custom", now.Format("2006-01-02 15:04:05"))
}
