package shapes_test

import (
	"gowithtdd/shapes"
	"testing"
)

func TestPerimeter(t *testing.T) {
	got := shapes.Perimeter(10.0, 10.0)
	want := 40.0

	if got != want {
		t.Errorf("got %.2f, want %.2f", got, want)
	}
}

func TestArea(t *testing.T) {

	got := shapes.Area(2.0, 3.0)
	want := 6.0

	if got != want {
		t.Errorf("got %.2f, want %.2f", got, want)
	}
}
