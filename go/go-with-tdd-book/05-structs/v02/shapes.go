package main

type Rectangle struct {
	Width float64
	Height float64
}

// Perimeter calculates the perimeter of a rectangle given
// its width and height.
func Perimeter(rect Rectangle) float64 {
  return 2 * (rect.Width + rect.Height)
}

// Area calculates the area of a rectangle given its
// width and height.
func Area(rect Rectangle) float64 {
  return rect.Width * rect.Height
}
