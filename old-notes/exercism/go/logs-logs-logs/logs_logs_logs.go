package logs

import "unicode/utf8"

// Application identifies the application emitting the given log.
func Application(log string) string {
	for _, chr := range log {
		switch chr {
		case '❗':
			return "recommendation"
		case '🔍':
			return "search"
		case '☀':
			return "weather"
		}
	}

	return "default"
}

// Replace replaces all occurrences of old with new, returning the
// modified log to the caller.
func Replace(log string, oldRune, newRune rune) string {
	var newLog string

	for _, chr := range log {
		if chr == oldRune {
			newLog += string(newRune)
		} else {
			newLog += string(chr)
		}
	}

	return newLog
}

// WithinLimit determines whether or not the number of characters in
// log is within the limit.
func WithinLimit(log string, limit int) bool {
	return utf8.RuneCountInString(log) <= limit
}
