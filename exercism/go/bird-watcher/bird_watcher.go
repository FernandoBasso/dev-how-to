package birdwatcher

// TotalBirdCount return the total bird count by summing the
// individual day's counts.
func TotalBirdCount(birdsPerDay []int) int {
	total := 0
	numDays := len(birdsPerDay)

	for i := 0; i < numDays; i++ {
		total += birdsPerDay[i]
	}

	return total
}

const daysInAWeek = 7

// BirdsInWeek returns the total bird count by summing only the
// items belonging to the given week.
//
// ASSUME: Weeks are always tracked completely, which means the
// slice length is always a multiple of 7.
func BirdsInWeek(birdsPerDay []int, week int) int {
	totalInWeek := 0
	startDay := (week - 1) * daysInAWeek
	endDay := week * daysInAWeek

	for i := startDay; i < endDay; i++ {
		totalInWeek += birdsPerDay[i]
	}

	return totalInWeek
}

// FixBirdCountLog returns the bird counts after correcting
// the bird counts for alternate days.
func FixBirdCountLog(birdsPerDay []int) []int {
	numDays := len(birdsPerDay)

	for i := 0; i < numDays; i += 2 {
		birdsPerDay[i]++
	}

	return birdsPerDay
}
