package greet1

import (
	"bytes"
	"testing"
)

func TestGreet(t *testing.T) {
	buf := bytes.Buffer{}
	Greet(&buf, "Yoda")

	want := "Hello, Yoda!"
	got := buf.String()

	if got != want {
		t.Errorf("got %s, want %s", got, want)
	}
}
