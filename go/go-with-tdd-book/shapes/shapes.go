package shapes

// Rectangle represents rectangle shapes.
type Rectangle struct {
	Width  float64
	Height float64
}

// Perimeter calculates the perimeter of a rectangle given
// its width and height.
func Perimeter(width, height float64) float64 {
	return 2 * (width + height)
}

// Area calculates the area of a rectangle given
// its width and height
func Area(width, height float64) float64 {
	return width * height
}
