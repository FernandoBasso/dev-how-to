package libhouse

type Director struct {
	builder IBuilder
}

type HouseInput struct {
	windowType string
	doorType   string
	floors     int
}

func newDirector(b IBuilder) *Director {
	return &Director{
		builder: b,
	}
}

func (d *Director) buildHouse(i HouseInput) House {
	d.builder.setWindowType(i.windowType)
	d.builder.setDoorType(i.doorType)
	d.builder.setFloors(i.floors)
	return d.builder.getHouse()
}
