package main

import (
	"bytes"
	"fmt"
)

// Greet writes name to the buffer.
func Greet(buf *bytes.Buffer, name string) {
	fmt.Fprintf(buf, name)
}
