package main

import (
	"fmt"
	"math"
)

type Shape interface {
	Area() float64
}

type Rectangle struct {
	Width  float64
	Height float64
}

type Circle struct {
	Radius float64
}

func (r Rectangle) Area() float64 {
	return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
	return r.Width*2 + r.Height*2
}

func (c Circle) Area() float64 {
	return math.Pi * math.Pow(c.Radius, 2)
}

func main() {
	r1 := Rectangle{Width: 3, Height: 4}
	fmt.Println(r1.Area())
	fmt.Println(r1.Perimeter())

	c1 := Circle{Radius: 3}
	fmt.Println(c1.Area())
}
