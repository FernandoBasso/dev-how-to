package iteration

const repeatCount = 5

// Repeat returns the input string repeated n times, or 5
// times if n is -1.
func Repeat(s string, n int) string {
	var repeated string

	lim := repeatCount

	if n >= 0 {
		lim = n
	}

	for i := 0; i < lim; i++ {
		repeated += s
	}

	return repeated
}
