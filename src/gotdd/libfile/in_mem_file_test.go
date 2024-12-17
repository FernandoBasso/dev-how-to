package libfile

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestInMemFile(t *testing.T) {
	t.Run("can create an instance of a file in memory", func(t *testing.T) {
		file := NewInMemFile([]byte("Hello, World!"))

		require.Equal(t, []byte("Hello, World!"), file.Content())
	})
}
