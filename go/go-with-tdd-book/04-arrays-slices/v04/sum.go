package main

// Sum takes a slice of ints and returns their sum.
func Sum(xs []int) int {
	total := 0

	for _, x := range xs {
		total += x
	}

	return total
}
