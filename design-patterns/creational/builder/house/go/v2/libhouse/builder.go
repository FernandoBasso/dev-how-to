package libhouse

type Builder interface {
	setWindowType(windowType string)
	setDoorType(doorType string)
	setFloors(floors int)
	getHouse() House
}
