package lib

import (
	"fmt"
	"io"
	"time"
)

type Sleeper interface {
	Sleep()
}

type DefaultSleeper struct{}

type ConfigurableSleeper struct {
	Duration time.Duration
	SleepFn  func(time.Duration)
}

func (c *ConfigurableSleeper) Sleep() {
	c.SleepFn(c.Duration)
}

func Countdown(dst io.Writer, sleeper Sleeper) {
	for c := 3; c > 0; c-- {
		fmt.Fprintf(dst, "%d\n", c)
		sleeper.Sleep()
	}

	fmt.Fprintf(dst, "%s", "Go!")
}
