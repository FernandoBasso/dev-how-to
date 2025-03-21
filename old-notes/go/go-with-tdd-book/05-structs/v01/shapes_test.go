package main

import "testing"

func TestPerimeter(t *testing.T) {
	got := Perimeter(10.0, 10.0)
	want := 40.0

	if got != want {
		t.Errorf("got %.2f\nwant %.2f", got, want)
	}
}

func TestArea(t *testing.T) {
	got := Area(2.0, 3.0)
	want := 6.0

	if got != want {
		t.Errorf("got %.2f\nwant %.2f", got, want)
	}
}
