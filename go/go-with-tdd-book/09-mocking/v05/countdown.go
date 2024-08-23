package main

import (
	"fmt"
	"io"
	"os"
	"time"
)

// Sleeper allows you to put delays.
type Sleeper interface {
	Sleep()
}

// DefaultSleeper is an implementation of Sleeper with a predefined delay.
type DefaultSleeper struct{}

// Sleep will pause execution for the defined Duration.
func (d *DefaultSleeper) Sleep() {
	time.Sleep(1 * time.Second)
}

// ConfigurableSleeper is an implementation of Sleeper with a defined delay.
type ConfigurableSleeper struct {
	duration time.Duration
	sleep    func(time.Duration)
}

func (c *ConfigurableSleeper) Sleep() {
  c.sleep(c.duration)
}

const (
	finalWord      = "Go!"
	countdownStart = 3
)

// Countdown prints a countdown with a delay between each step.
func Countdown(out io.Writer, sleeper Sleeper) {
	for i := countdownStart; i > 0; i-- {
		fmt.Fprintln(out, i)
		sleeper.Sleep()
	}

	fmt.Fprint(out, finalWord)
}

func main() {
	sleeper := &DefaultSleeper{}
	Countdown(os.Stdout, sleeper)
}
