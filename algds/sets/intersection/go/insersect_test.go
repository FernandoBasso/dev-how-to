package intersect

import (
	"reflect"
	"testing"
)

func TestIntersect(t *testing.T) {
	t.Run("two empty slices", func(t *testing.T) {
		got := Intersect([]int{}, []int{})
		want := []int{}

		if !reflect.DeepEqual(got, want) {
			t.Errorf("got #%v, want #%v", got, want)
		}
	})

	t.Run("empty left slice", func(t *testing.T) {
		got := Intersect([]int{}, []int{1, 2})
		want := []int{}

		if !reflect.DeepEqual(got, want) {
			t.Errorf("got #%v, want #%v", got, want)
		}
	})
}
