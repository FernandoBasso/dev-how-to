package main

import (
	"bytes"
	"fmt"
	"os"
	"testing"
)

func TestGreet(t *testing.T) {
	buffer := bytes.Buffer{}
	Greet(&buffer, "Yoda")

	got := buffer.String()
	want := "Yoda"

	if got != want {
	  t.Errorf("got %q, want %q", got, want)
	}
}

func ExampleGreet() {
	buf := bytes.Buffer{}
	Greet(&buf, "Aayla Secura")

	/* buf now contains the string. */

	fmt.Fprintf(os.Stdout, "%s\n", buf.String())
	// output: Aayla Secura
}
