package main

import (
	"reflect"
	"testing"
)

func TestSum(t *testing.T) {
	t.Run("collection of 5 ints", func(t *testing.T) {
		xs := []int{1, 2, 3, 4, 5}

		got := Sum(xs)
		want := 15

		if got != want {
			t.Errorf("got %d, want %d, given %v", got, want, xs)
		}
	})
}

func TestSumAll(t *testing.T) {
	got := SumAll([]int{1, 2}, []int{0, 9})
	want := []int{3, 9}

	if !reflect.DeepEqual(got, want) {
		t.Errorf("got %v, want %v", got, want)
	}
}

func TestAllTails(t *testing.T) {
	checkSums := func(t testing.TB, got, want []int) {
		t.Helper()

		if !reflect.DeepEqual(got, want) {
			t.Errorf("got %v, want %v", got, want)
		}
	}

	t.Run("sum the tails of empty slices", func(t *testing.T) {
		got := SumAllTails([]int {}, []int {1, 2, 3})
		want := []int{0, 5}

		checkSums(t, got, want)
	})

	t.Run("sum the tails of non-empty slices", func(t *testing.T) {
		got := SumAllTails([]int{1, 2}, []int{0, 9})
		want := []int{2, 9}

		checkSums(t, got, want)
	})
}
