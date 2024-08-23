package main

import (
	"bytes"
	"reflect"
	"testing"
	"time"
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

func TestConfigurableSleeper(t *testing.T) {
	sleepTime := 5 * time.Second

	spyTime := &SpyTime{}
	sleeper := ConfigurableSleeper{sleepTime, spyTime.Sleep}

	sleeper.Sleep()

	if spyTime.durationSlept != sleepTime {
	  t.Errorf("should have slept for %v, but slept for %v", sleepTime, spyTime.durationSlept)
	}
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

type SpyTime struct {
  durationSlept time.Duration
}

func (spy *SpyTime) Sleep(duration time.Duration) {
  spy.durationSlept = duration
}
