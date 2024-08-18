package main

import (
	"fmt"
	"io"
)

// Greet writes name to the buffer.
func Greet(writer io.Writer, name string) {
	fmt.Fprintf(writer, name)
}
