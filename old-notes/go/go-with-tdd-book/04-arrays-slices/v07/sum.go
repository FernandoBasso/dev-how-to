package main

// Sum takes a slice of ints and returns their sum.
func Sum(xs []int) int {
	total := 0

	for _, x := range xs {
		total += x
	}

	return total
}

// SumAll takes a variadic number of slices of ints and returns
// an array with the sum of each one.
func SumAll(numsToSum ...[]int) []int {
	var sums []int

	for _, nums := range numsToSum {
		sums = append(sums, Sum(nums))
	}

	return sums
}

// SumAllTails sums the tails of the variadic slices of ints
// and returns an arrays with their sum.
func SumAllTails(numsToSum ...[]int) []int {
	var sumsOfTails []int

	for _, nums := range numsToSum {
		tail := nums[1:]
		sumsOfTails = append(sumsOfTails, Sum(tail))
	}

	return sumsOfTails
}
