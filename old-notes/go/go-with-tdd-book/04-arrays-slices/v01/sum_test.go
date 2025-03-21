package main

import "testing"

func TestSum(t *testing.T) {
	xs := [5]int{1, 2, 3, 4, 5}

	got := Sum(xs)
	want := 15

	if got != want {
	  t.Errorf("got %d, want %d, given %v", got, want, xs)
	}
}
