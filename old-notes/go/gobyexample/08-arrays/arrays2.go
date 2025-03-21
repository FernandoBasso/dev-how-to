package main

import "fmt"

func main() {
  var xs [2]int
	xs[0] = 1
	xs[1] = 2
	fmt.Println(xs)

	// We can add type annotation before the assignment, but the type also
	// has to be part of the array itself after the assignment.
	var ys [2]int = [2]int{1, 2}
	fmt.Println(ys)
}
