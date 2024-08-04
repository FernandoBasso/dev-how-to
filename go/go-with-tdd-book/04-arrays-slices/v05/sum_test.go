package main

import (
	"testing"
	"reflect"
)

func TestSum(t *testing.T) {
	/*
	 * For this case of Sum(), a single test still keeps 100%
	 * coverage. We don't really need multiple tests for this
	 * specific case.
	 */

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
