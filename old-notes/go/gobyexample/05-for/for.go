package main

import "fmt"

func main() {
	i := 1
	for i <= 3 {
		fmt.Println(i)
		i = i + 1
	}

	fmt.Println()

	for j := 0; j < 3; j++ {
		fmt.Println(j)
	}

	fmt.Println()

	for k := range 3 {
		fmt.Println("range", k)
	}

	fmt.Println()

	for {
		fmt.Println("Loop and break!")
		break
	}

	fmt.Println()

	for n := range 6 {
		if n%2 == 0 {
			continue
		}

		fmt.Println(n)
	}
}
