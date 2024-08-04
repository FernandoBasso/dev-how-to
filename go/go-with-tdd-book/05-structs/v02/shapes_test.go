package main

import "testing"

func TestPerimeter(t *testing.T) {
	rect := Rectangle{10.0, 10.0}
	got := Perimeter(rect)
	want := 40.0

	if got != want {
		t.Errorf("got %.2f\nwant %.2f", got, want)
	}
}

func TestArea(t *testing.T) {
	rect := Rectangle{2.0, 3.0}
	got := Area(rect)
	want := 6.0

	if got != want {
		t.Errorf("got %.2f\nwant %.2f", got, want)
	}
}
