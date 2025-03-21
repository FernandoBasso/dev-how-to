package main

import (
	"bytes"
	"testing"
)

func TestCountdown(t *testing.T) {
	buf := &bytes.Buffer{}

	Countdown(buf)

	got := buf.String()
	want := `3
2
1
Go!`

	if got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}
