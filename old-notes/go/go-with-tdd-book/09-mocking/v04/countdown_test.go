package main

import (
	"bytes"
	"reflect"
	"testing"
)

func TestCountdown(t *testing.T) {
	t.Run("prints 3 to Go!", func(t *testing.T) {
		buf := &bytes.Buffer{}
		spySleeper := &SpyCountdownOps{}

		Countdown(buf, spySleeper)

		got := buf.String()
		want := `3
2
1
Go!`

		if got != want {
			t.Errorf("got %q, want %q", got, want)
		}
	})

	t.Run("sleep in-between prints", func(t *testing.T) {
		spySleeperPrinter := &SpyCountdownOps{}
		Countdown(spySleeperPrinter, spySleeperPrinter)

		want := []string{
			"write",
			"sleep",
			"write",
			"sleep",
			"write",
			"sleep",
			"write",
		}

		if !reflect.DeepEqual(want, spySleeperPrinter.Calls) {
			t.Errorf("want calls %q, got %q", want, spySleeperPrinter.Calls)
		}
	})
}

type SpyCountdownOps struct {
	Calls []string
}

func (spy *SpyCountdownOps) Sleep() {
	spy.Calls = append(spy.Calls, sleep)
}

func (spy *SpyCountdownOps) Write(p []byte) (n int, err error) {
	spy.Calls = append(spy.Calls, write)
	return
}

const (
	write = "write"
	sleep = "sleep"
)
