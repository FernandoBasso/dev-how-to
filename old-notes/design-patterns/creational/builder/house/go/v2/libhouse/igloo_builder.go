package libhouse

type IglooBuilder struct {
	windowType string
	doorType   string
	floors     int
}

func (b *IglooBuilder) setWindowType(windowType string) {
	b.windowType = windowType
}
func (b *IglooBuilder) setDoorType(doorType string) {
	b.doorType = doorType
}
func (b *IglooBuilder) setFloors(floors int) {
	b.floors = floors
}

func newIglooBuilder() *IglooBuilder {
	return new(IglooBuilder)
}

func (b *IglooBuilder) getHouse() House {
	return House{
		windowType: b.windowType,
		doorType:   b.doorType,
		floors:     b.floors,
	}
}
