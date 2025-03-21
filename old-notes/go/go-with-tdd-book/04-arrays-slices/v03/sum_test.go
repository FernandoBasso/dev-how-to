package main

import "testing"

func TestSum(t *testing.T) {
	t.Run("collection of 5 ints", func(t *testing.T) {
		xs := []int{1, 2, 3, 4, 5}

		got := Sum(xs)
		want := 15

		if got != want {
			t.Errorf("got %d, want %d, given %v", got, want, xs)
		}
	})

	t.Run("collection of any size", func(t *testing.T) {
		xs := []int{1, 2, 3, 4, 5, 6}

		got := Sum(xs)
		want := 21

		if got != want {
			t.Errorf("got %d, want %d, given %v", got, want, xs)
		}
	})
}
