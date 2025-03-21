package main

import "math"

// Shape is implemented by anything that can tell us its area.
type Shape interface {
	Area() float64
}

// Rectangle has the dimensions of a rectangle
type Rectangle struct {
	Width  float64
	Height float64
}

// Circle has the radio of a circle.
type Circle struct {
	Radius float64
}

// Triangle represents the dimensions of a triangle.
type Triangle struct {
	Base   float64
	Height float64
}

// Area returns the area of a rectangle.
func (r Rectangle) Area() float64 {
	return r.Width * r.Height
}

// Area returns the area of a triangle.
func (t Triangle) Area() float64 {
	return (t.Base * t.Height) * 0.5;
}

// Perimeter returns the perimeter of a rectangle.
func Perimeter(rect Rectangle) float64 {
	return 2 * (rect.Width + rect.Height)
}

// Area returns the area of a circle.
func (c Circle) Area() float64 {
	return math.Pi * c.Radius * c.Radius
}
