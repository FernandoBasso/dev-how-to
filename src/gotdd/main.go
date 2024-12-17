package main

import (
	"bytes"
	"compress/gzip"
	"fmt"
	"os"
)

func main() {
	content := []byte("Hello, World! From Gzip!!\n\n\n")
	buffer := bytes.NewBuffer([]byte{})
	gzip.NewWriter(buffer).Write(content)

	os.WriteFile("./out.gz", content, 0644)
	fmt.Printf("%#v", buffer)
}
