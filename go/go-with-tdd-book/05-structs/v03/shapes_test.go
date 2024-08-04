package main

import "testing"

func TestPerimeter(t *testing.T) {
	rect := Rectangle{10.0, 10.0}
	got := rect.Perimeter(rect)
	want := 40.0

	if got != want {
		t.Errorf("got %.2f\nwant %.2f", got, want)
	}
}

func TestArea(t *testing.T) {
	t.Run("rectangles", func(t *testing.T) {
		rect := Rectangle{2.0, 3.0}
		got := rect.Area(rect)
		want := 6.0

		if got != want {
			t.Errorf("got %g\nwant %g", got, want)
		}
	})

	t.Run("circles", func(t *testing.T) {
		circle := Circle{10}
		got := circle.Area(circle)
		want := 314.1592653589793

		if got != want {
			t.Errorf("got %g\nwant %g", got, want)
		}
	})
}
