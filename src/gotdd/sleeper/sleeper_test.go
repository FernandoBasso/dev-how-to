package sleeper

import (
	"testing"
	"time"

	"github.com/stretchr/testify/require"
)

func TestSleeper(t *testing.T) {
	t.Run("sleeps for the intended amount of time", func(t *testing.T) {
		timeToSleep := 1 * time.Second
		spyTime := SpyTime{}
		sleeper := CustomSleeper{Duration: timeToSleep, SleepFn: spyTime.Sleep}

		sleeper.Sleep()

		require.Equal(t, spyTime.timeSlept, timeToSleep)
	})

	t.Run("sleeps multiple times and accumulates the total time slept", func(t *testing.T) {
		timeToSleep := 1 * time.Second
		spyTime := SpyTime{}
		sleeper := CustomSleeper{Duration: timeToSleep, SleepFn: spyTime.Sleep}

		sleeper.Sleep()
		sleeper.Sleep()
		sleeper.Sleep()

		require.Equal(t, spyTime.timeSlept, 3*timeToSleep)
	})
}

type SpyTime struct {
	timeSlept time.Duration
}

func (s *SpyTime) Sleep(duration time.Duration) {
	s.timeSlept += duration
}
