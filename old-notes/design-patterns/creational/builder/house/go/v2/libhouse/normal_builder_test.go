package libhouse

import (
	"reflect"
	"testing"
)

func TestNormalBuilder(t *testing.T) {
	t.Run("can create a zero-valued builder", func(t *testing.T) {
		got := newNormalBuilder()
		want := &NormalBuilder{
			windowType: "",
			doorType:   "",
			floors:     0,
		}

		if !reflect.DeepEqual(got, want) {
			t.Errorf("got #%v, want #%v", got, want)
		}
	})

	t.Run("implements a getHouse() method", func(t *testing.T) {
		actualHouse := newNormalBuilder().getHouse()
		expectedHouse := House{
			windowType: "",
			doorType:   "",
			floors:     0,
		}

		if !reflect.DeepEqual(actualHouse, expectedHouse) {
			t.Errorf("got #%v, want #%v", actualHouse, expectedHouse)
		}
	})

	t.Run("implements the setter methods of the builder interface", func(t *testing.T) {
		nb := newNormalBuilder()

		nb.setWindowType("Wooden")
		nb.setDoorType("Wooden")
		nb.setFloors(2)

		actualHouse := nb.getHouse()
		expectedHouse := House{
			windowType: "Wooden",
			doorType:   "Wooden",
			floors:     2,
		}

		if !reflect.DeepEqual(actualHouse, expectedHouse) {
			t.Errorf("got #%v, want #%v", actualHouse, expectedHouse)
		}
	})
}
