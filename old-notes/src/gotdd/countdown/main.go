package main

import (
	"main/lib"
	"os"
	"time"
)

func main() {
	lib.Countdown(
		os.Stdout,
		&lib.ConfigurableSleeper{Duration: 4 * time.Second, SleepFn: time.Sleep},
	)
}
