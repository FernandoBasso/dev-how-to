package main

import "fmt"

// Rect represents a rectangular shape.
type Rect struct {
	Width float64
	Height float64
}

// Area computes the area of the given rectangle.
func Area(rect Rect) float64 {
  return rect.Width * rect.Height
}

// Shape is implemented by anything that can tell us its area.
type Shape interface {
	Area() float64
}

// Area is a method on Rect which gives us the area of the Rect.
func (rect Rect) Area() float64 {
  return rect.Width * rect.Height
}

func main() {
  fmt.Printf("%8.2f\n", Area(Rect{2, 3}))
	fmt.Printf("%8.2f\n", Rect{2, 3}.Area())
}
