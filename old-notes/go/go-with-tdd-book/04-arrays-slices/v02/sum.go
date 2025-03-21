package main

// Sum takes an array of five ints and returns their sum.
func Sum(xs [5]int) int {
	total := 0

	for _, x := range xs {
	  total += x
	}

	return total
}
