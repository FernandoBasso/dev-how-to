package booking

import "time"

// Schedule returns a time.Time from a string containing a date.
func Schedule(date string) time.Time {
	t, err := time.Parse("1/2/2006 15:04:05", date)

	if err != nil {
		panic(err)
	}

	return t
}

// HasPassed returns whether a date has passed.
func HasPassed(date string) bool {
	t, err := time.Parse("January 2, 2006 15:04:05", date)

	if err != nil {
		panic(err)
	}

	return time.Now().After(t)
}

// IsAfternoonAppointment returns whether a time is in the afternoon.
func IsAfternoonAppointment(date string) bool {
	// Thursday, May 13, 2010 20:32:00
	t, err := time.Parse("Monday, January 2, 2006 15:04:05", date)

	if err != nil {
		panic(err)
	}

	hour := t.Hour()

	return 12 <= hour && hour < 18
}

// Description returns a formatted string of the appointment time.
func Description(date string) string {
	// "You have an appointment on Thursday, July 25, 2019, at 13:45."
	// "7/25/2019 13:45:00"
	parsed := Schedule(date).Format("Monday, January 2, 2006, at 15:04.")
	return "You have an appointment on " + parsed
}

// AnniversaryDate returns a Time with this year's anniversary.
func AnniversaryDate() time.Time {
	return time.Date(time.Now().Year(), time.September, 15, 0, 0, 0, 0, time.UTC)
}
