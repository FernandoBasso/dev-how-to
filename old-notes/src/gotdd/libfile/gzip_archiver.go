package libfile

import (
	"bytes"
	"compress/gzip"
)

func GzipCompress(content []byte) []byte {
	gzippedBuf := bytes.NewBuffer([]byte{})
	gzipWriter := gzip.NewWriter(gzippedBuf)

	gzipWriter.Write(content)
	gzipWriter.Close()

	return gzippedBuf.Bytes()
}
