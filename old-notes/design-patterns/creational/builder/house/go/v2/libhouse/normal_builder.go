package libhouse

type NormalBuilder struct {
	windowType string
	doorType   string
	floors     int
}

func (b *NormalBuilder) setWindowType(windowType string) {
	b.windowType = windowType
}
func (b *NormalBuilder) setDoorType(doorType string) {
	b.doorType = doorType
}
func (b *NormalBuilder) setFloors(floors int) {
	b.floors = floors
}

func newNormalBuilder() *NormalBuilder {
	return new(NormalBuilder)
}

func (b *NormalBuilder) getHouse() House {
	return House{
		windowType: b.windowType,
		doorType:   b.doorType,
		floors:     b.floors,
	}
}
