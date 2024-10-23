package libhouse

import (
	"reflect"
	"testing"
)

func TestDirector(t *testing.T) {
	t.Run("when passed builder type does not exist", func(t *testing.T) {
		nonExistingBuilder := getBuilder("non-existing-builder")

		if nonExistingBuilder != nil {
			t.Errorf("want nil builder, got #%v", nonExistingBuilder)
		}
	})

	t.Run("can work with a normal builder", func(t *testing.T) {
		normalBuilder := getBuilder("normal")
		director := newDirector(normalBuilder)

		houseInput := HouseInput{
			windowType: "Wood",
			doorType:   "Wood",
			floors:     2,
		}

		actualHouse := director.buildHouse(houseInput)
		expectedHouse := House(houseInput)

		if !reflect.DeepEqual(actualHouse, expectedHouse) {
			t.Errorf("want house #%v, got #%v", expectedHouse, actualHouse)
		}
	})

	t.Run("can work with an igloo builder", func(t *testing.T) {
		iglooBuilder := getBuilder("igloo")
		director := newDirector(iglooBuilder)

		houseInput := HouseInput{
			windowType: "Ice",
			doorType:   "Ice",
			floors:     1,
		}

		actualHouse := director.buildHouse(houseInput)
		expectedHouse := House(houseInput)

		if !reflect.DeepEqual(actualHouse, expectedHouse) {
			t.Errorf("want house #%v, got #%v", expectedHouse, actualHouse)
		}
	})
}
