package main

import "fmt"

func main() {
	normalBuilder := getBuilder("normal")

	director := newDirector(normalBuilder)

	normalHouse := director.buildHouse()
	fmt.Printf("#%v\n", normalHouse)

	director.setBuilder(getBuilder("igloo"))

	iglooHouse := director.buildHouse()
	fmt.Printf("#%v\n", iglooHouse)
}
