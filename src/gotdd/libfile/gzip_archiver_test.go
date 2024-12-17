package libfile

import (
	"bytes"
	"compress/gzip"
	"io"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestGzipArchiver(t *testing.T) {
	t.Run("can perform gzip compression", func(t *testing.T) {
		input := []byte("Hello, GZIP!")

		gzipped := GzipCompress(input)

		bytesReader := bytes.NewReader(gzipped)
		gzipReader, _ := gzip.NewReader(bytesReader)
		ioReadAll, _ := io.ReadAll(gzipReader)

		require.Equal(t, input, ioReadAll)
	})
}
