package booking

import "time"

// Schedule returns a time.Time from a string containing a date.
//
// Example input: 7/13/2020 20:32:00
func Schedule(date string) time.Time {
	t, err := time.Parse("1/2/2006 15:04:05", date)

	if err != nil {
		panic(err)
	}

	return t
}

// HasPassed returns whether a date has passed.
//
// Example input: October 3, 2019 20:32:00
func HasPassed(date string) bool {
	t, err := time.Parse("January 2, 2006 15:04:05", date)

	if err != nil {
		panic(err)
	}

	return time.Now().After(t)
}

// IsAfternoonAppointment returns whether a time is in the afternoon.
//
// Example input: Thursday, May 13, 2010 20:32:00
func IsAfternoonAppointment(date string) bool {
	t, err := time.Parse("Monday, January 2, 2006 15:04:05", date)

	if err != nil {
		panic(err)
	}

	hour := t.Hour()

	return 12 <= hour && hour < 18
}

// Description returns a formatted string of the appointment time.
//
// Example input: 6/6/2005 10:30:00
func Description(date string) string {
	formatted := Schedule(date).Format("Monday, January 2, 2006, at 15:04.")
	return "You have an appointment on " + formatted
}

// AnniversaryDate returns a Time with this year's anniversary.
//
// What this exercise wants is that we basically return a date from
// September 15 of the current year.
func AnniversaryDate() time.Time {
	return time.Date(time.Now().Year(), time.September, 15, 0, 0, 0, 0, time.UTC)
}
