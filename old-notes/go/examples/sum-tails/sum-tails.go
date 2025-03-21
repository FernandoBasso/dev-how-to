package main

import "fmt"

func Sum(xs []int) int {
	len := len(xs)
	total := 0

	for i := 0; i < len; i++ {
		total += xs[i]
	}

	return total
}

// fn :: [Int] -> [Int]

// as, bs, cs
func SumAllTails(listOfXs ...[]int) []int {
	var sums []int

	for _, xs := range listOfXs {
		tail := xs[1:]
		sums = append(sums, Sum(tail))
	}

	return sums
}

func main() {
	fmt.Println(SumAllTails([]int{1, 2, 3}, []int{1, 2, 3}))
}
