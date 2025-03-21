package lib

import (
	"bytes"
	"reflect"
	"testing"
	"time"
)

func TestCountdown(t *testing.T) {
	t.Run("prints from 3 to Go!", func(t *testing.T) {
		out := bytes.Buffer{}

		Countdown(&out, &SpyOps{})

		got := out.String()
		want := `3
2
1
Go!`

		if got != want {
			t.Errorf("got %q, want %q", got, want)
		}
	})

	t.Run("writes and sleeps in the correct order", func(t *testing.T) {
		spyOps := SpyOps{}

		Countdown(&spyOps, &spyOps)

		got := spyOps.Calls
		want := []string{"write", "sleep", "write", "sleep", "write", "sleep", "write"}

		if !reflect.DeepEqual(got, want) {
			t.Errorf("got %v, want %v", got, want)
		}
	})
}

func TestConfigurableSleeper(t *testing.T) {
	t.Run("sleeps for the expected amount of time", func(t *testing.T) {
		sleepTime := 4 * time.Second
		spyTime := &SpyTime{}
		sleeper := ConfigurableSleeper{sleepTime, spyTime.Sleep}
		sleeper.Sleep()

		if spyTime.timeSlept != sleepTime {
			t.Errorf("slept for %d seconds, should have been %d", spyTime.timeSlept, sleepTime)
		}
	})
}

type SpyOps struct {
	Calls []string
}

func (s *SpyOps) Sleep() {
	s.Calls = append(s.Calls, "sleep")
}

func (s *SpyOps) Write(p []byte) (n int, err error) {
	s.Calls = append(s.Calls, "write")
	return
}

type SpyTime struct {
	timeSlept time.Duration
}

func (s *SpyTime) Sleep(duration time.Duration) {
	s.timeSlept = duration
}
