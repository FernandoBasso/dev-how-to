package iteration

const repeatCount = 5

// Repeat returns the input string repeated 5 times.
func Repeat(s string) string {
	var repeated string

	for i := 0; i < repeatCount; i++ {
		repeated += s
	}

	return repeated
}
