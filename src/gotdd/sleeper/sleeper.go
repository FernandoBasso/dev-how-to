package sleeper

import "time"

type Sleeper interface {
	Sleep()
}

type CustomSleeper struct {
	Duration time.Duration
	SleepFn  func(time.Duration)
}

func (cs CustomSleeper) Sleep() {
	cs.SleepFn(cs.Duration)
}
