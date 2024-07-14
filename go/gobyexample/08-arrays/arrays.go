package main

import "fmt"

func main() {
  var a [5]int
	fmt.Println("emp:", a)

	a[4] = 100
	fmt.Println(a)
	fmt.Println(a[4])

	fmt.Println("len:", len(a))

	b := [5]int{1, 2, 3, 4, 5}
	fmt.Println("b:", b)

	c := [...]int{10, 20, 30}
	fmt.Println("c:", c)

	d := [...]int{100, 3: 400, 500}
	fmt.Println("d:", d)

	fmt.Println()

	var twoD [2][3]int
	for i := 0; i < 2; i++ {
		for j := 0; j < 3; j++ {
		  twoD[i][j] = i + j
		}
	}
	fmt.Println("twoD:", twoD)

	fmt.Println()

	var two = [2][3]int{
		{10, 20, 30},
		{100, 200, 300},
	}
	fmt.Println(two)
}
