package libfile

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestCompressedFile(t *testing.T) {
	t.Run("creates new instance of a compressed file", func(t *testing.T) {
		compressedFile := NewCompressedFile(
			NewInMemFile([]byte("example content")),
			noOp,
		)

		require.Equal(
			t,
			[]byte("example content"),
			compressedFile.Content(),
		)
	})
}

func noOp(content []byte) []byte {
	return content
}
