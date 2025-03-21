package libhouse

import (
	"reflect"
	"testing"
)

func TestBuilder(t *testing.T) {
	t.Run("returns nil when kind is not a valid, known one", func(t *testing.T) {
		b := getBuilder("unknown-kind")

		if b != nil {
			t.Errorf("got #%v, want nil", b)
		}
	})

	t.Run("returns a normal builder", func(t *testing.T) {
		b := getBuilder("normal")

		b.setWindowType("wooden window")
		b.setDoorType("wooden door")
		b.setFloors(2)

		actualHouse := b.getHouse()

		expectedHouse := House{
			windowType: "wooden window",
			doorType:   "wooden door",
			floors:     2,
		}

		if !reflect.DeepEqual(actualHouse, expectedHouse) {
			t.Errorf("got #%v, want #%v", actualHouse, expectedHouse)
		}
	})

	t.Run("returns a igloo builder", func(t *testing.T) {
		b := getBuilder("igloo")

		b.setWindowType("ice window")
		b.setDoorType("ice door")
		b.setFloors(1)

		actualHouse := b.getHouse()

		expectedHouse := House{
			windowType: "ice window",
			doorType:   "ice door",
			floors:     1,
		}

		if !reflect.DeepEqual(actualHouse, expectedHouse) {
			t.Errorf("got #%v, want #%v", actualHouse, expectedHouse)
		}
	})
}
