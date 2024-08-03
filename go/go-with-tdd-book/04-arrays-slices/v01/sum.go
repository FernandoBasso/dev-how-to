package main

func Sum(xs [5]int) int {
	total := 0

	for i := 0; i < 5; i++ {
		total += xs[i]
	}

	return total
}
