package main

import "math"

/*
 * NOTE: Had to add the type as a param in Area() and Perimeter(). Those
 * were not present in the examples in the book when I studied it in
 * 2024. I'm using go 1.22.5 as of this writing.
 */

// Rectangle has the dimensions of a rectangle
type Rectangle struct {
	Width  float64
	Height float64
}

// Circle has the radio of a circle.
type Circle struct {
	Radius float64
}

// Area returns the area of a rectangle.
func (r Rectangle) Area(Rectangle) float64 {
	return r.Width * r.Height
}

// Perimeter returns the perimeter of a rectangle.
func (r Rectangle) Perimeter(Rectangle) float64 {
	return 2 * (r.Width + r.Height)
}

// Area returns the area of a circle.
func (c Circle) Area(Circle) float64 {
	return math.Pi * c.Radius * c.Radius
}

