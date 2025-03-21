package libhouse

import (
	"reflect"
	"testing"
)

func TestIglooBuilder(t *testing.T) {
	t.Run("can create a zero-valued builder", func(t *testing.T) {
		got := newIglooBuilder()
		want := &IglooBuilder{
			windowType: "",
			doorType:   "",
			floors:     0,
		}

		if !reflect.DeepEqual(got, want) {
			t.Errorf("got #%v, want #%v", got, want)
		}
	})

	t.Run("implements a getHouse() method", func(t *testing.T) {
		actualHouse := newIglooBuilder().getHouse()
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
		nb := newIglooBuilder()

		nb.setWindowType("Ice")
		nb.setDoorType("Ice")
		nb.setFloors(2)

		actualHouse := nb.getHouse()
		expectedHouse := House{
			windowType: "Ice",
			doorType:   "Ice",
			floors:     2,
		}

		if !reflect.DeepEqual(actualHouse, expectedHouse) {
			t.Errorf("got #%v, want #%v", actualHouse, expectedHouse)
		}
	})
}
